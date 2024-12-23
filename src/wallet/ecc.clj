(ns wallet.ecc
  (:require [wallet.util :as util])
  (:import [org.bouncycastle.crypto.params ECPrivateKeyParameters ECDomainParameters]
           [org.bouncycastle.crypto.ec CustomNamedCurves])  )

(defrecord PrivateKey [key chain-code version fingerprint depth child-index])

(defn make-private-key [key chain-code version fingerprint depth child-index]
  (->PrivateKey key chain-code version
                (if (int? fingerprint)
                  (util/->n-byte-array 0 4)
                  fingerprint)
                depth
                child-index))

(defn private-key? [k]
  (instance? PrivateKey k))

(defn validate-private-key
  [^bytes key-bytes]
  (when-let [msg (cond (not= 32 (count key-bytes)) "The key byte size msut be 32"

                       (not (< 0 (BigInteger. 1 key-bytes)
                               (-> "secp256k1"
                                   CustomNamedCurves/getByName
                                   .getN)))
                       "The key is out of the EC range")]
    (throw (ex-info "Key validation failed, a new seed required" {:reason msg}))))

(defrecord PublicKey [key chain-code version fingerprint depth child-index])

(defn public-key? [k]
  (instance? PublicKey k))

(defn make-public-key
  ([m]
   (map->PublicKey m))
  ([key chain-code version fingerprint depth child-index]
   (->PublicKey key chain-code version fingerprint depth child-index)))

(defn raw-private-key->public-key [raw-private-key]
  (-> "secp256k1"
      CustomNamedCurves/getByName
      .getG
      (.multiply (BigInteger. 1 raw-private-key))
      (.getEncoded true)))

(defn validate-public-key
  [^bytes key-bytes]
  (when (not= 33 (count key-bytes))
    (throw (ex-info "Wrong byte size for xpub" {:size (count key-bytes)})))
  (try (-> (CustomNamedCurves/getByName "secp256k1")
           .getCurve
           (.decodePoint key-bytes))
       (catch Exception e
         (println "???" e)
         (throw (ex-info "Out of EC for xpub" {:key-bytes (vector key-bytes)})))))
