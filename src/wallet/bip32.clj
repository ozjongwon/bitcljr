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
        curve-order (.getN curve)
        domain (ECDomainParameters. (.getCurve curve) ;; The actual curve equation parameters
                                    (.getG curve) ;; generator point (base point), the public key
                                    curve-order ;; the order of the curve ,for the private key
                                    ;;(.getH curve)
                                    )
        private-key (ECPrivateKeyParameters. (BigInteger. 1 key-bytes) domain)
        pkey-big-int (.getD private-key)]
    (if  (< 0 pkey-big-int curve-order)
      private-key
      (throw (ex-info "Key validation failed, new seed required" {:private-key pkey-big-int})))))

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
         chain-code (byte-array (drop 32 raw))
         ;; Create private key from first 32 bytes
         private-key (create-private-key private-bytes)]
     ;; 4. Generating the Master Public Key
     ;; Using the secp256k1 elliptic curve, the master private key is used to compute the corresponding master public key.
     ;; This involves a point multiplication operation on the secp256k1 curve, resulting in a public key.

     ;; 5. Output
     ;; The output is a structure containing:
     ;; The master private key.
     ;; The corresponding master public key.
     ;; The chain code.
     ;; Together, these components form the root of the HD wallet, enabling the derivation of child keys (both private and public) in a deterministic way.

     (->HDPrivateKey private-key chain-code version))))

(defn- int->n-byte-array [i n]
  (let [bytes (byte-array n)]
    (run! #(aset-byte bytes % (bit-and (bit-shift-right i (* 8 (- n %))) 0xff)) (range 4))
    bytes))


(defn private-key->xprv [{:keys [private-key chain-code version]}]
  (let [depth 0
        parent-fingerprint 0
        child-index 0
        raw-key-bytes (let [key-bytes (-> (.getD private-key) (.toByteArray))
                            len (count key-bytes)]
                        (case len
                          33 key-bytes
                          (31 32) (let [padded-bytes (byte-array 33)
                                        n-bytes (- 33 len)]
                                    (dotimes [i n-bytes]
                                      (aset-byte padded-bytes i 0x00))
                                    (System/arraycopy key-bytes 0 padded-bytes n-bytes (min 32 (count key-bytes)))
                                    padded-bytes)))]
    ;; Check with
    ;;https://learnmeabitcoin.com/technical/keys/hd-wallets/extended-keys/
    ;; (println "***"
    ;;          (->> chain-code (map #(format "%02x" (bit-and % 0xFF))) (apply str))
    ;;          (->> raw-key-bytes (map #(format "%02x" (bit-and % 0xFF))) (apply str))
    ;;          (->> `[~@(vec version) ; 4 bytes
    ;;                 ~depth          ; 1 byte
    ;;                 ~@(vec (int->n-byte-array parent-fingerprint 4)) ; 4 bytes
    ;;                 ~@(vec (int->n-byte-array child-index 4)) ; 4 bytes
    ;;                 ~@(vec chain-code)                        ; 32 bytes
    ;;                 ~@(vec raw-key-bytes)]
    ;;               (map #(format "%02x" (bit-and % 0xFF)))
    ;;               (apply str)))
    (b58/encode-check (byte-array `[~@(vec version) ; 4 bytes
                                    ~depth          ; 1 byte
                                    ~@(vec (int->n-byte-array parent-fingerprint 4)) ; 4 bytes
                                    ~@(vec (int->n-byte-array child-index 4)) ; 4 bytes
                                    ~@(vec chain-code)                        ; 32 bytes
                                    ~@(vec raw-key-bytes)]))))
