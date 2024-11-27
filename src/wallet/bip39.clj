(ns wallet.bip39
  (:require [clojure.java.io :as io]
            [buddy.core.hash :as hash]
            [buddy.core.kdf :as kdf]
            [buddy.core.codecs :as codecs]
            [buddy.core.mac :as mac]
            [wallet.base58 :as b58]
            [wallet.bip32 :as b32]
            [wallet.networks :as net])
  (:import [org.bouncycastle.crypto.params ECPrivateKeyParameters ECDomainParameters]
           [org.bouncycastle.crypto.ec CustomNamedCurves]
           [org.bouncycastle.asn1.sec SECNamedCurves]
           [org.bouncycastle.math.ec ECPoint]))

(def ^:dynamic *lang-key*)

;;;
;;; Read the orignal wordlist files from 'resources'
;;;
(defn- load-wordlists [filenames]
  (into {}
        (for [filename filenames]
          [(-> filename
               (io/file)
               (.getName)
               (clojure.string/replace #"\.txt$" "")
               keyword)

           (with-open [reader (io/reader filename)]
             (vec (line-seq reader)))])))

(defn- list-txt-files [folder-path]
  (let [folder (io/file folder-path)]
    (when (.isDirectory folder)
      (->> (.listFiles folder)
           (filter #(.isFile %))       ;; Keep only files
           (filter #(-> % .getName (clojure.string/ends-with? ".txt"))) ;; Keep .txt files
           (map #(.getAbsolutePath %)))))) ;; Return absolute paths for each file

(defonce +word-list+ (->> "resources"
                          list-txt-files
                          load-wordlists))

(defn- remove-first-n-bits [n num num-bits]
  ;; get num-of-bits, shift left, dec, shift-right, bit-and
  (-> (bit-shift-left 1 num-bits)
      (dec)
      (bit-shift-right n)
      (bit-and num)))

(defonce +word-bit-lengh+ 11)

(defn extract-11-bits-dynamic [bytev n-words]
  (loop [[byte & more-bytes] bytev
         carry 0
         carry-bits 0
         result []
         count n-words]
    (if (= count 0)
      result
      (let [combined (bit-or (bit-shift-left carry 8) (bit-and byte 0xff)) ; Merge carry and current byte
            n-available-bits (+ carry-bits 8)] ; Total available bits
        (if (>= n-available-bits +word-bit-lengh+)
          ;; If we have enough bits to extract an 11-bit value
          (let [shift-amount (- n-available-bits +word-bit-lengh+) ; Calculate how many bits to shift to extract the 11 bits
                extracted-bits (bit-and (bit-shift-right combined shift-amount) 0x7FF) ; Extract the 11 bit
                leftover-value (remove-first-n-bits +word-bit-lengh+ combined n-available-bits)] ; Get remaining bits
            (recur more-bytes
                   leftover-value
                   shift-amount
                   (conj result extracted-bits)
                   (dec count))) ; Add the 11 bits to the result
          ;; Not enough bits yet, so we accumulate more from the next byte
          (recur more-bytes
                 combined
                 n-available-bits
                 result
                 count))))))

(defn- add-checksum [bytev]
  (let [len (count bytev)]
    ;;    (assert (zero? (mod len 16)))
    (->> bytev
         byte-array
         hash/sha256
         (into bytev)
         byte-array)))

(defn pack-11-bits-dynamic [nums]
  (loop [[num & more-nums] nums
         carry 0
         carry-bits 0
         result []]
    (cond (<= 8 carry-bits)
          (let [offset (- carry-bits 8)]
            (recur (cons num more-nums)
                   (remove-first-n-bits 8 carry carry-bits)
                   offset
                   (conj result (bit-shift-right carry offset))))
          num
          (let [offset (- 8 carry-bits)
                to-byte (- +word-bit-lengh+ offset) ]
            (recur more-nums
                   (remove-first-n-bits offset num +word-bit-lengh+)
                   to-byte
                   (conj result (bit-or (bit-shift-left carry offset) (bit-shift-right num to-byte)))))

          :finally  (if (and (zero? carry) (zero? carry-bits))
                      result
                      (->> carry-bits
                           (- 8)
                           (bit-shift-left carry)
                           (conj result))))))

(defn mnemonic->bytes
  ([mnemonics]
   (mnemonic->bytes mnemonics *lang-key*))
  ([mnemonics lang-key]
   (let [len (count mnemonics)
         _ (assert (<= 4 (quot len 3) 8)) ;; 12, ... 24 words
         wd-list (get +word-list+ lang-key)
         binary-seed (->> mnemonics
                          (map #(if-let [idx (.indexOf wd-list %)]
                                  idx
                                  (throw (ex-info "Word is not in the dictionary!" {:word %}))))
                          pack-11-bits-dynamic)
         checksum-length-bits (quot (* len 11) 33)
         num-remainder (rem checksum-length-bits 8)
         [checksum-length bits-to-ignore] (if (zero? num-remainder)
                                            [(quot checksum-length-bits 8) 0]
                                            [(-> checksum-length-bits (quot 8) inc)
                                             (- 8 num-remainder)])
         [data checksum] [(subvec binary-seed 0 (- (count binary-seed) checksum-length))
                          (subvec binary-seed (- (count binary-seed) checksum-length))]
         computed-checksum (subvec (vec (hash/sha256 (byte-array data))) 0 checksum-length)]

     (when (not= (first checksum) (-> computed-checksum
                                      first
                                      (bit-and 0xff)
                                      (bit-shift-right bits-to-ignore)
                                      (bit-shift-left
                                       bits-to-ignore)))
       (throw (ex-info "Checksum verification failed" {:expect (first computed-checksum)
                                                       :actual (first checksum)
                                                       :bits-to-ignore bits-to-ignore
                                                       :mnemonics mnemonics})))

     data)))


(defn indices->mnemonic [indices]
  (map #(get-in +word-list+ [*lang-key* %]) indices))

(defn bytes->mnemonic [entropy]
  (let [len (count entropy)
        _ (assert (<= 4 (quot len 4) 8))
        total-bits (* len 8)
        checksum-bits (quot total-bits 32)
        total-mnemonics (-> (+ total-bits checksum-bits)
                            (quot +word-bit-lengh+))
        checksum (-> (if (bytes? entropy)
                       entropy
                       (byte-array entropy))
                     (hash/sha256))]
    (-> (if (bytes? entropy)
          (vec entropy)
          entropy) ;; vector!
        (into checksum)
        (extract-11-bits-dynamic total-mnemonics)
        indices->mnemonic)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mnemonics->pbkdf2-sha512-kdf [mnemonics pw]
  (let [pbkdf2+sha512 (kdf/engine {:alg :pbkdf2+sha512
                                   :key (codecs/bytes->str mnemonics)
                                   :salt (str "mnemonic" pw)
                                   :iterations 2048})]
    (-> (kdf/get-bytes pbkdf2+sha512 64)
        (codecs/bytes->hex))))

(defn mnemonic->seed
  ([mnemonic]
   (mnemonic->seed mnemonic nil))
  ([mnemonic passwd]
   (mnemonic->seed  mnemonic passwd :english))
  ([mnemonic passwd lang-key]
   ;; only supports 12, 24, ...
   ;;   (assert (zero? (mod (count mnemonic) 12)))
   (let [wd-list (get +word-list+ lang-key)
         seed-bytes (->> (map #(.indexOf wd-list %) mnemonic)
                         pack-11-bits-dynamic)]
     (mnemonics->pbkdf2-sha512-kdf seed-bytes passwd))


   ;; return hashlib.pbkdf2_hmac(
   ;;                            "sha512",
   ;;                            mnemonic.encode("utf-8"),
   ;;                            ("mnemonic" + password).encode("utf-8"),
   ;;                            PBKDF2_ROUNDS,
   ;;                            64,
   ;;                            )



   ;; @classmethod
   ;; def from_seed(cls, seed: bytes, version=NETWORKS["main"]["xprv"]):
   ;; """Creates a root private key from 64-byte seed"""
   ;; raw = hmac.new(b"Bitcoin seed", seed, digestmod="sha512").digest()
   ;; private_key = ec.PrivateKey(raw[:32])
   ;; chain_code = raw[32:]
   ;; return cls(private_key, chain_code, version=version)

   ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(comment
  (for [[seed mnemonic hex-seed xprv] test-data
        :let [seed (-> (clojure.string/split mnemonic #" ")
                       (mnemonic->seed "TREZOR")
                       (wallet.bip32/seed->hd-key)
                       (wallet.bip32/encode-extended-key))]]
    [hex-seed seed])
  )


(comment
  (ns crypto.debug
    (:require [buddy.core.mac :as mac]
              [buddy.core.codecs :as codecs]
              [clojure.string :as str]))

  ;; First, let's make the seed generation explicit
  (def test-mnemonic "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about")

  (defn mnemonic->seed [mnemonic password]
    (let [salt (str "mnemonic" (or password ""))
          result (buddy.core.kdf/get-bytes
                  (buddy.core.kdf/engine
                   {:alg :pbkdf2
                    :digest :sha512
                    :key (.getBytes mnemonic "UTF-8")
                    :salt (.getBytes salt "UTF-8")
                    :iterations 2048})
                  64)]
      result))

  (defn create-domain-params []
    (let [curve secp256k1-curve]
      (ECDomainParameters.
       (.getCurve curve)    ; The curve equation
       (.getG curve)        ; Generator point
       (.getN curve))))     ; Field order

  (defn debug-master-key-generation []
    (let [ ;; 1. Generate seed
          seed (mnemonic->seed test-mnemonic "")
          _ (println "Seed (hex):" (codecs/bytes->hex seed))

          ;; 2. Generate HMAC-SHA512
          hmac-key (.getBytes "Bitcoin seed" "UTF-8")
          raw (mac/hash seed {:key hmac-key :alg :hmac+sha512})
          _ (println "HMAC (hex):" (codecs/bytes->hex raw))

          ;; 3. Split into key and chain code
          private-bytes (byte-array (take 32 raw))
          chain-code (byte-array (drop 32 raw))
          _ (println "Private key bytes (hex):" (codecs/bytes->hex private-bytes))
          _ (println "Chain code (hex):" (codecs/bytes->hex chain-code))

          ;; 4. Create EC private key
          ;;domain (create-domain-params)
          private-key (b32/create-private-key private-bytes)

          ;; 5. Create final structure
          master-key {:private-key private-key
                      :chain-code chain-code
                      :version (get-in net/+networks+ ["main" "xprv"])}

          ;; 6. Encode
          encoded (b32/encode-extended-key master-key)]

      {:seed (codecs/bytes->hex seed)
       :raw-hmac (codecs/bytes->hex raw)
       :private-key-hex (codecs/bytes->hex private-bytes)
       :chain-code-hex (codecs/bytes->hex chain-code)
       :base58 encoded}))
  )
