(ns wallet.bip85
  (:require [wallet.bip32 :as b32]
            [wallet.bip39 :as b39]
            [buddy.core.crypto :as crypto]
            [buddy.core.mac :as mac]
            [buddy.core.codecs :as codecs]))

(defonce +lang-vector+
  [:english :japanese :korean :spanish :chinese-simplified
   :chinese-traditional :french :italian :czech :portuguese])

(defn derive-entropy [root path]
  (let [hardened-path (->> `[83696968 ~@path]
                           (mapv (fn [part]
                                   (let [hardened (+ part b32/+hardened-index+)]
                                     (assert (<= b32/+hardened-index+ hardened 0xffffffff))
                                     hardened))))]
    (-> root
        (b32/path->child hardened-path)
        :key
        (mac/hash {:key "bip-entropy-from-k" :alg :hmac+sha512})
        vec)))

(defn derive-mnemonic
  ([root-key]
   (derive-mnemonic root-key :english))
  ([root-key lang-key]
   (derive-mnemonic root-key lang-key 12))
  ([root-key lang-key num-words]
   (derive-mnemonic root-key lang-key num-words 0))
  ([root-key lang-key num-words index]
   (assert (contains? #{12 18 24} num-words))
   (assert (< index b32/+hardened-index+))
   (-> root-key
       (derive-entropy [39 (.indexOf +lang-vector+ lang-key) num-words index])
       (subvec
        0
        (quot (* num-words 4) 3))
       byte-array
       b39/bytes->mnemonic)))

;;(derive-mnemonic :root)

(defn derive-wif
  ([root]
   (derive-wif root 0))
  ([root index]
   (->> [2 index]
        (derive-entropy root)
        ;;(make-private-key)
        )))

;;(derive-wif :root)

(defn derive-xprv
  ([root]
   (derive-xprv root 0))
  ([root index]
   (->> [32 index]
        (derive-entropy root))))

;;(derive-xprv :root)

(defn derive-hex
  ([root]
   (derive-hex root 32))
  ([root num-bytes]
   (derive-hex root num-bytes 0))
  ([root num-bytes index]
   (assert (<= 16 num-bytes 64))
   (->> [128169 num-bytes index]
        (derive-entropy root))))
;;(derive-hex :root)

(comment
  (defn derive-pwd-base64
    ([root]
     (derive-pwd-base64 root 20))
    ([root pwd-len]
     (derive-pwd-base64 root pwd-len 0))
    ([root pwd-len index]
     (assert (<= 20 pwd-len 86))
     (->> [707764 pwd-len index]
          (derive-entropy root))))

  (defn derive-pwd-base85
    ([root]
     (derive-pwd-base85 root 20))
    ([root pwd-len]
     (derive-pwd-base85 root pwd-len 0))
    ([root pwd-len index]
     (assert (<= 10 pwd-len 80))
     (->> [707785 pwd-len index]
          (derive-entropy root))))

  (defn derive-rsa
    ([root]
     (derive-rsa root ??))
    ([root key-bits]
     (derive-rsa root key-bits ???))
    ([root key-bits key-index]
     (->> [828365 key-bits key-index]
          (derive-entropy root))))

  (defn derive-rsa-gpg
    ([root]
     (derive-rsa-gpg root ??))
    ([root key-bits]
     (derive-rsa-gpg root key-bits ???))
    ([root key-bits key-index]
     (->> [83696968 key-bits key-index]
          (derive-entropy root))))

  (defn derive-dice [])
  )
