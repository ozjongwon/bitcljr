(ns bitclojr.ecc
  (:require [bitclojr.util :as util])
  (:import [org.bouncycastle.crypto.params ECPrivateKeyParameters ECDomainParameters]
           [org.bouncycastle.crypto.ec CustomNamedCurves])  )

(defrecord PrivateKey [key chain-code version fingerprint depth child-index])

(defn make-private-key [key chain-code version fingerprint depth child-index]
  ;; note: key can be bytes
  (->PrivateKey key
                chain-code
                version
                (cond (int? fingerprint) (util/->n-vector fingerprint 4)
                      (sequential? fingerprint) fingerprint)
                depth
                child-index))

(defn private-key? [k]
  (instance? PrivateKey k))

(defn validate-private-key
  [key-bytes-or-vec]
  (when-let [msg (cond (not= 32 (count key-bytes-or-vec)) "The key byte size msut be 32"

                       (not (< 0 (BigInteger. 1 ^bytes (util/ensure-bytes key-bytes-or-vec))
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
   ;; note: key can be bytes
   (->PublicKey key chain-code version fingerprint depth child-index)))

(defn derive-secp256k1-public-key
  ([raw-private-key]
   (derive-secp256k1-public-key raw-private-key nil))
  ([raw-private-key scalar-to-add]
   (let [g (-> "secp256k1" CustomNamedCurves/getByName .getG)]
     (-> g
         (.multiply (BigInteger. 1  ^bytes (util/ensure-bytes raw-private-key))) ;; pub key1
         (cond->                                     ;; pub key2 (by adding a point)
             scalar-to-add (.add (->> (BigInteger. 1 ^bytes scalar-to-add)
                                      (.multiply g))))
         (.getEncoded true)))))

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
