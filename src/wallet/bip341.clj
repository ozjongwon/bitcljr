(ns wallet.bip341
  (:require [clojure.string :as str]
            [buddy.core.mac :as mac]
            [buddy.core.hash :as hash]
            [wallet.base58 :as b58]
            [wallet.networks :as net]
            [wallet.ecc :as ecc]
            [buddy.core.codecs :as codecs])
  (:import [org.bouncycastle.crypto.params ECPrivateKeyParameters ECDomainParameters]
           [org.bouncycastle.crypto.ec CustomNamedCurves]))

(defprotocol Bip341Taproot
  (taproot-tweak [this] [this h]))

(defn tagged-hash [tag data]
  (let [hash-tag (hash/sha256 tag)]
    (hash/sha256 (byte-array `[~@hash-tag ~@hash-tag ~@data]))))

(defn %taproot-tweak [k h]
  (let [tweak (tagged-hash "TapTweak" `[~@k ~@(.getBytes h)])]
    (ecc/validate-private-key tweak)
    ;; compressed prefix - 0x02 == y coord is pos(or even), 0x03 == neg(odd)
    (-> (byte-array `[0x02 ~@k])
        (ecc/derive-secp256k1-public-key tweak)
        rest
        (#(ecc/make-public-key {:key %})))))

(extend-type wallet.ecc.PrivateKey
  Bip341Taproot
  (taproot-tweak
    ([this]
     (taproot-tweak this ""))
    ([{:keys [key]} h]
     (%taproot-tweak key h))))

(extend-type wallet.ecc.PublicKey
  Bip341Taproot
  (taproot-tweak
    ([this]
     (taproot-tweak this ""))
    ([{:keys [key]} h]
     (%taproot-tweak key h))))
