(ns wallet.bip32
  (:require [clojure.string :as str]
            [buddy.core.mac :as mac]
            [buddy.core.hash :as hash]
            [wallet.base58 :as b58]
            [wallet.networks :as net]
            [wallet.ecc :as ecc]
            [wallet.util :as util]
            [buddy.core.codecs :as codecs])
  (:import [org.bouncycastle.crypto.params ECPrivateKeyParameters ECDomainParameters]
           [org.bouncycastle.crypto.ec CustomNamedCurves]))

;;;
;;; Key making and validating
;;;
(defn private-key->public-key [private-key]
  (assert (ecc/private-key? private-key))
  (ecc/make-public-key (assoc private-key
                              :key (ecc/raw-private-key->public-key (:key private-key))
                              :version (get-in net/+networks+ ["main" "xpub"]))))

(defn seed->hd-key
  "Creates a root private key from 64-byte seed
   seed: byte array of exactly 64 bytes
   version: (optional) network version bytes, defaults to mainnet xprv"
  ([seed]
   (seed->hd-key seed (get-in net/+networks+ ["main" "xprv"])))
  ([seed version] ;; Generate HMAC-SHA512 of the seed
   (let [raw (mac/hash seed {:key "Bitcoin seed" :alg :hmac+sha512})
         ;; Split into private key and chain code
         private-bytes (byte-array (take 32 raw))
         chain-code (byte-array (drop 32 raw))]
     (ecc/validate-private-key private-bytes)
     (ecc/make-private-key private-bytes chain-code version 0 0 0))))

;;;
;;; HD (Hierarchical Deterministic) Key from a mnemonic phrase and an optional password
;;;
(defonce +hardened-index+ 0x80000000)

(defn hardened-index? [i]
  (>= i +hardened-index+))

(defn hash160 [x]
  (-> x hash/sha256 hash/ripemd160))

(defn parse-path [path]
  ;; "m/44h/1'/0'/0/32"
  (let [path-list (str/split path #"/")]
    (for [i (if (= (first path-list) "m") ; master key?
              (rest path-list)
              path-list)
          :let [hardened? (contains? #{\' \h \H} (last i))]]
      (if hardened?
        (-> (subs i 0 (dec (count i))) (Integer/parseInt) (+ +hardened-index+))
        (Integer/parseInt i)))))

(defprotocol Bip32HDKey
  (make-child-data-bytes [parent index])
  (make-hd-key [parent raw-bytes]))

(extend-type wallet.ecc.PrivateKey
  Bip32HDKey
  (make-child-data-bytes [{:keys [key]} index]
    ;; only hardened or not matters, not public/private
    (-> (if (hardened-index? index)
          `[~@(repeat (- 33 (count key)) 0x00)
            ~@key ~@(util/->n-byte-array index 4)]
          `[~@(ecc/raw-private-key->public-key key) ~@(util/->n-byte-array index 4)])
        (byte-array)))

  (make-hd-key [{:keys [key]} secret]
    (->> "secp256k1"
         CustomNamedCurves/getByName
         ( .getN)
         (.mod (.add (BigInteger. 1 key) (BigInteger. 1 secret)))
         .toByteArray)))

(extend-type wallet.ecc.PublicKey
  Bip32HDKey
  (make-child-data-bytes [{:keys [key] :as parent} index]
    (when (hardened-index? index)
      (throw (ex-info "Can't derive a hardened key from a public key"
                      {:parent parent :index index})))
    (byte-array `[~@key ~@(util/->n-byte-array index 4)]))

  (make-hd-key [{:keys [key]} raw-bytes] ;; parent-key child-raw-bytes
    (let [curve (CustomNamedCurves/getByName "secp256k1")]
      (-> curve
          .getCurve
          (.decodePoint key)
          (.add (.multiply (.getG curve) (BigInteger. 1 raw-bytes)))
          (.getEncoded true)))))

(defn key->version [key]
  (if (= (count key) 32)
    (get-in net/+networks+ ["main" "xprv"])
    (get-in net/+networks+ ["main" "xpub"])))

(defn make-child-key [key chain-code version fingerprint depth index]
  (let [keylen (count key)
        maybe-new-key (if (and (> keylen 32) (= (first key) 0x00))
                        (let [zeros (take (- keylen 32) key)]
                          (assert (every? zero? zeros))
                          (->> key (drop (- keylen 32)) byte-array))
                        key)]
    (if (<= (count maybe-new-key) 32)
      (ecc/make-private-key maybe-new-key chain-code (get-in net/+networks+ ["main" "xprv"])
                            fingerprint depth index)
      (ecc/make-public-key maybe-new-key chain-code (get-in net/+networks+ ["main" "xpub"])
                           fingerprint depth index))))

(defn ec-key->fingerprint [ec-key]
  (let [{:keys [key]} (if (ecc/private-key? ec-key)
                        (private-key->public-key ec-key)
                        ec-key)]
    (->> key hash160 (take 4) byte-array)))

;;https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#user-content-Private_parent_key_rarr_private_child_key
(defn derive-child [{:keys [key chain-code depth] :as parent} index]
  (when (> index 0xFFFFFFFF)
    (throw (ex-info "Index must be: index <= 2^32" {:index index})))
  (let [raw (mac/hash (make-child-data-bytes parent index) {:key chain-code :alg :hmac+sha512})
        ;; Split into key and chain code
        raw-bytes (byte-array (take 32 raw))
        child-chain-code (byte-array (drop 32 raw))
        _ (assert (and (= (count raw-bytes) 32)
                       (= (count child-chain-code) 32)))
        child-key (make-hd-key parent raw-bytes)]
    (make-child-key child-key
                    child-chain-code
                    (key->version child-key)
                    (ec-key->fingerprint parent)
                    (inc depth)
                    index)))

(defn path->child [k path]
  (loop [[idx & more] (if (string? path)
                        (parse-path path)
                        path)
         parent k]
    (if idx
      (recur more (derive-child parent idx))
      parent)))

(defn encode-hd-key [{:keys [key chain-code version depth version fingerprint child-index] :as key-record}]
  (let [raw-key-bytes (let [len (count key)]
                        (case len
                          33 key

                          (31 32)
                          (let [padded-bytes (byte-array 33)
                                n-bytes (- 33 len)]
                            (dotimes [i n-bytes]
                              (aset-byte padded-bytes i 0x00))
                            (->> key
                                 count
                                 (min 32)
                                 (System/arraycopy key 0 padded-bytes n-bytes))
                            padded-bytes)))]
    ;; Check with
    ;;https://learnmeabitcoin.com/technical/keys/hd-wallets/extended-keys/
    (b58/encode-check (byte-array `[~@(vec version) ; 4 bytes
                                    ~depth          ; 1 byte
                                    ~@(vec fingerprint)    ; 4 bytes
                                    ~@(vec (util/->n-byte-array child-index 4)) ; 4 bytes
                                    ~@(vec chain-code) ; 32 bytes
                                    ~@(vec raw-key-bytes)])
                      :bytes)))

(defn decode-hd-key [hd-key-str]
  (let [vprefix (subs hd-key-str 0 4)]
    (if-let [ver (->> vprefix
                      (conj ["main"])
                      (get-in net/+networks+))]
      (let [[version depth fingerprint index chain-code raw-key]
            (loop [bytes (b58/decode-check hd-key-str :bytes) [n-bytes & more-n-bytes] [4 1 4 4 32] result []]
              (if n-bytes
                (recur (drop n-bytes bytes) more-n-bytes (conj result (take n-bytes bytes)))
                (conj result bytes)))
            depth-int (util/bytes->int depth)
            index-int (util/bytes->int index)
            fingerprint-int (BigInteger. 1 (byte-array fingerprint))
            non-zero-raw-key (drop-while zero? raw-key)
            raw-key-byte (byte-array raw-key)]
        (cond  (and (= "xpub" vprefix) (not= 33 (count raw-key-byte)))
               (throw (ex-info "Wrong byte size for xpub" {:size (count raw-key-byte)}))

               (and (= "xpub" vprefix) (not (contains? #{0x02 0x03} (first non-zero-raw-key))))
               (throw (ex-info "Wrong prefix for xpub" {:prefix (first non-zero-raw-key)}))

               (and (= "xpub" vprefix)
                    (try (do (-> (CustomNamedCurves/getByName "secp256k1")
                                 .getCurve
                                 (.decodePoint raw-key-byte))
                             false)
                         (catch Exception e
                           true)))
               (throw (ex-info "Out of EC for xpub" {:raw-key raw-key}))

               (and (= "xprv" vprefix) (not (<= 31 (count non-zero-raw-key) 32)))
               (throw (ex-info "Wrong byte size or prefix for xprv" {:non-zero-raw-key non-zero-raw-key}))

               (and (= "xprv" vprefix)
                    (not (< 0 (BigInteger. 1 raw-key-byte) (-> "secp256k1"
                                                               CustomNamedCurves/getByName
                                                               .getN))))
               (throw (ex-info "The privkey is out of the EC range" {:key-int (BigInteger. 1 raw-key-byte)}))


               (not (<= 0 depth-int 255))
               (throw (ex-info "Depth out of range" {:depth (first depth-int)}))

               (and (zero? depth-int) (not= fingerprint-int 0))
               (throw (ex-info "Invalid master key depth fingerprint" {:fingerprint fingerprint-int}))

               (and (zero? depth-int) (not (zero? index-int)))
               (throw (ex-info "Invalid master key depth with child index" {:child-index index-int})))
        ((case vprefix
           "xprv" ecc/make-private-key
           "xpub" ecc/make-public-key)
         raw-key-byte (byte-array chain-code) ver (byte-array fingerprint) depth-int index-int))
      (throw (ex-info "Unknown version" {:version (subs hd-key-str 0 4)})))))
