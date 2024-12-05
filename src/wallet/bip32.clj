(ns wallet.bip32
  (:require [clojure.string :as str]
            [buddy.core.mac :as mac]
            [buddy.core.hash :as hash]
            [wallet.base58 :as b58]
            [wallet.networks :as net]
            [buddy.core.codecs :as codecs])
  (:import [org.bouncycastle.crypto.params ECPrivateKeyParameters ECDomainParameters]
           [org.bouncycastle.crypto.ec CustomNamedCurves]))
;;;
;;; Utils
;;;

(defn- unsigned->signed [val]
  (if (> val 127)
    (- val 256)
    val))

(defn- ->n-byte-array [i n]
  (if (bytes? i)
    i
    (let [bytes (byte-array n)]
      (loop [idx (dec n) v i]
        (if (< idx 0)
          bytes
          (do (->> 0xff
                   (bit-and v)
                   unsigned->signed
                   (aset-byte bytes idx))
              (recur (dec idx) (bit-shift-right v 8))))))))

;;;
;;; Key making and validating
;;;

(defn validate-private-key
  [^bytes key-bytes]
  (when-let [msg (cond (not= 32 (count key-bytes)) "The key byte size msut be 32"

                       (not (< 0 (BigInteger. 1 key-bytes)
                               (-> "secp256k1"
                                   CustomNamedCurves/getByName
                                   .getN)))
                       "The key is out of the EC range")]
    (throw (ex-info "Key validation failed, a new seed required" {:reason msg}))))

(declare private-key?)

(defn raw-private-key->public-key [raw-private-key]
  (-> "secp256k1"
      CustomNamedCurves/getByName
      .getG
      (.multiply (BigInteger. 1 raw-private-key))
      (.getEncoded true)))

(declare make-hd-public-key)

(defn private-key->public-key [private-key]
  (assert (private-key? private-key))
  (make-hd-public-key (assoc private-key
                             :key (raw-private-key->public-key (:key private-key))
                             :version (get-in net/+networks+ ["main" "xpub"]))))

(declare make-hd-private-key)

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
     (validate-private-key private-bytes)
     (make-hd-private-key private-bytes chain-code version 0 0 0))))

;;;
;;; HD (Hierarchical Deterministic) Key from a mnemonic phrase and an optional password
;;;
(defonce +hardened-index+ 0x80000000)

(defn hardened-index? [i]
  (>= i +hardened-index+))

(defprotocol HDKey
  (make-child-data-bytes [parent index])
  (make-hd-key [parent raw-bytes]))

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

(defrecord HDPrivateKey [key chain-code version fingerprint depth child-index]
  HDKey
  (make-child-data-bytes [parent index]
    ;; only hardened or not matters, not public/private
    (-> (if (hardened-index? index)
          `[0x0 ~@key ~@(->n-byte-array index 4)]
          `[~@(raw-private-key->public-key key) ~@(->n-byte-array index 4)])
        (byte-array)))

  (make-hd-key [parent secret]
    (->> "secp256k1"
         CustomNamedCurves/getByName
         ( .getN)
         (.mod (.add (BigInteger. 1 key) (BigInteger. 1 secret)))
         .toByteArray)))

(defn make-hd-private-key [key chain-code version fingerprint depth child-index]
  (->HDPrivateKey key chain-code version
                  (if (int? fingerprint)
                    (->n-byte-array 0 4)
                    fingerprint)
                  depth
                  child-index))

(defrecord HDPublicKey [key chain-code version fingerprint depth child-index]
  HDKey
  (make-child-data-bytes [this index]
    (when (hardened-index? index)
      (throw (ex-info "Can't derive a hardened key from a public key"
                      {:parent this :index index})))
    (byte-array `[~@key ~@(->n-byte-array index 4)]))

  (make-hd-key [parent raw-bytes] ;; parent-key child-raw-bytes
    (let [curve (CustomNamedCurves/getByName "secp256k1")
          parent-point (.decodePoint (.getCurve curve) key)]
      (-> (.add parent-point (.multiply (.getG curve) (BigInteger. 1 raw-bytes)))
          (.getEncoded true)))))

(defn public-key? [k]
  (instance? HDPublicKey k))

(defn make-hd-public-key
  ([m]
   (map->HDPublicKey m))
  ([key chain-code version fingerprint depth child-index]
   (->HDPublicKey key chain-code version fingerprint depth child-index)))

(defn private-key? [k]
  (instance? HDPrivateKey k))

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
      (make-hd-private-key maybe-new-key chain-code (get-in net/+networks+ ["main" "xprv"])
                           fingerprint depth index)
      (make-hd-public-key maybe-new-key chain-code (get-in net/+networks+ ["main" "xpub"])
                          fingerprint depth index))))

(defn ec-key->fingerprint [ec-key]
  (let [{:keys [key]} (if (instance? HDPrivateKey ec-key)
                        (private-key->public-key ec-key)
                        ec-key)]
    (->> key hash160 (take 4) byte-array)))

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
                                    ~@(vec (->n-byte-array child-index 4)) ; 4 bytes
                                    ~@(vec chain-code) ; 32 bytes
                                    ~@(vec raw-key-bytes)]))))
