(ns wallet.bip39
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [buddy.core.hash :as hash]
            [buddy.core.kdf :as kdf]
            [buddy.core.codecs :as codecs]))

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

(defn indices->mnemonic
  ([indices]
   (indices->mnemonic indices *lang-key*))
  ([indices lang-key]
   (if-let [wd-list (get +word-list+ lang-key)]
     (->> indices
          (map #(if-let [found (get-in +word-list+ [lang-key %])]
                  found
                  (throw (ex-info "Out of index!" {:index %}))))
          (interpose " ")
          (apply str))
     (throw (ex-info "Unknown language" {:lang-key lang-key})))))

(defn mnemonic->indices
  ([mnemonic]
   (mnemonic->indices mnemonic *lang-key*))
  ([mnemonic lang-key]
   (if-let [wdlist (get +word-list+ lang-key)]
     (map #(if-let [found (.indexOf wdlist %)]
             found
             (throw (ex-info "Word is not in the dictionary!" {:word %})))
          mnemonic)
     (throw (ex-info "Unknown language" {:lang-key lang-key}))))  )

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
  ([mnemonic]
   (mnemonic->bytes mnemonic *lang-key*))
  ([mnemonic lang-key]
   (let [words (str/split mnemonic #" ")
         len (count words)
         _ (assert (<= 4 (quot len 3) 8)) ;; 12, ... 24 words
         binary-seed (-> words (mnemonic->indices) pack-11-bits-dynamic)
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
                                                       :mnemonic mnemonic})))

     (byte-array data))))

(defn bytes->mnemonic [entropy]
  (let [len (count entropy)
        _ (assert (<= 4 (quot len 4) 8))
        total-bits (* len 8)
        checksum-bits (quot total-bits 32)
        total-mnemonic (-> (+ total-bits checksum-bits)
                            (quot +word-bit-lengh+))
        checksum (-> (if (bytes? entropy)
                       entropy
                       (byte-array entropy))
                     (hash/sha256))]
    (-> (if (bytes? entropy)
          (vec entropy)
          entropy) ;; vector!
        (into checksum)
        (extract-11-bits-dynamic total-mnemonic)
        indices->mnemonic)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- mnemonic->pbkdf2-sha512-kdf [mnemonic pw]
  (->  {:alg :pbkdf2+sha512
        :key mnemonic
        :salt (str "mnemonic" pw)
        :iterations 2048}
       kdf/engine
       (kdf/get-bytes 64)
       codecs/bytes->hex))

(defn mnemonic->seed
  ([mnemonic]
   (mnemonic->seed mnemonic nil))
  ([mnemonic passwd]
   (mnemonic->seed  mnemonic passwd :english))
  ([mnemonic passwd lang-key]
   (assert (<= 4 (-> mnemonic
                     (str/split #" ")
                     count
                     (quot 3)) 8))
   (mnemonic->pbkdf2-sha512-kdf mnemonic passwd)))
