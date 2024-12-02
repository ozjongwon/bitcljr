(ns wallet.bip32
  (:require [clojure.string :as str]
            [buddy.core.mac :as mac]
            [wallet.base58 :as b58]
            [wallet.networks :as net])
  (:import [org.bouncycastle.crypto.params ECPrivateKeyParameters ECDomainParameters]
           [org.bouncycastle.crypto.ec CustomNamedCurves]))

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
                                    )
        private-key (ECPrivateKeyParameters. (BigInteger. 1 key-bytes) domain)
        pkey-big-int (.getD private-key)]
    (if  (< 0 pkey-big-int curve-order)
      (.toByteArray pkey-big-int)
      (throw (ex-info "Key validation failed, a new seed required" {:private-key pkey-big-int})))))

(defn private-key->public-key [private-key-bytes]
  (-> "secp256k1"
      CustomNamedCurves/getByName
      .getG
      (.multiply (BigInteger. 1 private-key-bytes))
      (.getEncoded false)))

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
     (->HDPrivateKey private-bytes chain-code version 0 0 0))))

(defn private-key->xprv [{:keys [key chain-code version depth fingerprint]}]
  (let [child-index 0
        raw-key-bytes (let [len (count key)]
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
                                    ~@(vec (int->n-byte-array fingerprint 4)) ; 4 bytes
                                    ~@(vec (int->n-byte-array child-index 4)) ; 4 bytes
                                    ~@(vec chain-code)                        ; 32 bytes
                                    ~@(vec raw-key-bytes)]))))
