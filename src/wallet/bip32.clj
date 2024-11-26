(ns wallet.bip32
  (:require [clojure.java.io :as io]
            [buddy.core.hash :as hash]
            [buddy.core.kdf :as kdf]
            [buddy.core.codecs :as codecs]
            [buddy.core.mac :as mac]
            [wallet.base58 :as b58]
            [wallet.networks :as net])
  (:import [org.bouncycastle.crypto.params ECPrivateKeyParameters ECDomainParameters]
           [org.bouncycastle.crypto.ec CustomNamedCurves]
           [org.bouncycastle.asn1.sec SECNamedCurves]
           [org.bouncycastle.math.ec ECPoint]))

;;;
;;; HD (Hierarchical Deterministic) Key from a mnemonic phrase and an optional password
;;;

(defrecord HDPrivateKey [private-key chain-code version])

(defn create-private-key
  "Creates EC private key from 32 bytes"
  [^bytes key-bytes]
  (let [curve (CustomNamedCurves/getByName "secp256k1") ;; deterministic
        domain (ECDomainParameters. (.getCurve curve) ;; The actual curve equation parameters
                                    (.getG curve)     ;; generator point (base point) of secp256k1
                                    (.getN curve))]   ;; the order of the curve (the size of the finite field)
    (ECPrivateKeyParameters. (BigInteger. 1 key-bytes) ;; Private key must be a positive number less than the curve order
                             domain)))

(defn seed->hd-key
  "Creates a root private key from 64-byte seed
   seed: byte array of exactly 64 bytes
   version: (optional) network version bytes, defaults to mainnet xprv"
  ([seed]
   (from-seed seed (get-in net/+networks+ ["main" "xprv"])))
  ([seed version] ;; Generate HMAC-SHA512 of the seed
   (let [raw (mac/hash seed {:key "Bitcoin seed" :alg :hmac+sha512})
         ;; Split into private key and chain code
         private-bytes (byte-array (take 32 raw))
         chain-code (byte-array (drop 32 raw))
         ;; Create private key from first 32 bytes
         private-key (create-private-key private-bytes)]
     (->HDPrivateKey private-key chain-code version))))

(defn- double-sha256
  "Perform SHA256(SHA256(input))"
  [bytes]
  (-> bytes
      hash/sha256
      hash/sha256))

(defonce +base58-alphabet+ (vec "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"))

(defn- encode-base58check
  "Encode bytes with version prefix and checksum in base58"
  [data]
  (let [checksum (take 4 (double-sha256 data))
        with-checksum (byte-array (concat data checksum))
        zero-count (count (take-while zero? data))
        leading-ones (apply str (repeat zero-count "1"))]
    (->> with-checksum
         biginteger
         (#(loop [n % acc []]
             (if (pos? n)
               (recur (quot n 58)
                      (cons (nth +base58-alphabet+ (mod n 58)) acc))
               (apply str leading-ones acc)))))))
#_
(defn encode-extended-key
  "Encode BIP32 extended private key"
  [{:keys [private-key chain-code version]}]
  ;; Concatenate all components
  (-> (byte-array (concat version
                          [0] ;; Depth byte (1 byte) - 0x00 for master key
                          [0 0 0 0] ;; Parent fingerprint (4 bytes) - 0x00000000 for master key
                          [0 0 0 0] ;; Child number (4 bytes) - 0x00000000 for master key
                          chain-code ;; Chain code (32 bytes)
                          (into [0] ;; Private key with 0x00 prefix (33 bytes)
                                (-> private-key
                                    .getD
                                    .toByteArray
                                    (#(if (> (count %) 32)
                                        (drop (- (count %) 32) %)
                                        %))))))
      encode-base58check))

(defn encode-extended-key [{:keys [private-key chain-code version]}]
  (let [;; Version bytes - explicit byte ordering
        version-bytes version
        ;; Ensure private key is exactly 32 bytes
        key-bytes (let [kb (.toByteArray (.getD private-key))
                        len (count kb)
                        result (byte-array 32)]
                    (if (> len 32)
                      (System/arraycopy kb (- len 32) result 0 32)
                      (System/arraycopy kb 0 result (- 32 len) len))
                    result)
        ;; Build the full 78 bytes
        serialized (byte-array (concat version-bytes           ;4 bytes
                                       [(byte 0)]               ;depth
                                       (byte-array 4)           ;parent fingerprint
                                       (byte-array 4)           ;child number
                                       chain-code               ;32 bytes
                                       [(byte 0)]               ;private key prefix
                                       key-bytes))              ;32 bytes
        ;; Add checksum
        checksum (take 4 (double-sha256 serialized))
        final-bytes (byte-array (concat serialized checksum))]
    ;; Convert to base58
    (->> final-bytes
         (BigInteger. 1)
         (#(loop [n % acc []]
             (if (pos? n)
               (recur (quot n 58)
                      (cons (nth +base58-alphabet+ (mod n 58)) acc))
               (apply str acc))))
         (#(str (apply str (repeat (count (take-while zero? serialized)) "1")) %)))))

;; Example usage:
(comment
  (let [master-key {:private-key private-key-obj
                    :chain-code chain-code-bytes
                    :version 0x0488ADE4}  ; mainnet version
        encoded (encode-extended-key master-key)]
    ;; Results in string like "xprv9s21ZrQH143K..."
    encoded))
