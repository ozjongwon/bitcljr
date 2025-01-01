(ns bitclojr.wallet
  (:require [bitclojr.bip44 :as b44]
            [bitclojr.bip32 :as b32]
            [bitclojr.ecc :as ecc]
            [bitclojr.networks :as net]
            [bitclojr.util :as util]
            [buddy.core.codecs :as codecs]
            [clojure.string :as str]
            ;; [bitclojr.util :as util]
            ;; [bitclojr.bip39 :as b39]


            ;; [buddy.core.crypto :as crypto]
            ;; [buddy.core.nonce :as nonce]
            ;; [clojure.edn :as edn]
            ;; [clj-cbor.core :as cbor]
            ;; [buddy.core.hash :as hash]


            )
  ;;(:import [java.util Base64 Arrays])
  )

(defrecord WalletConfig [gap-limit]
  ;;  {:gap-limit 20}
  )

(defrecord Address [address script-type derivation-path address-type script-policy label]
  ;; :script-type - how to spend or create, use to generate addresses
  ;; :derivation-path - to derive address
  ;; :address-type -to construct address
  ;; :script-policy #{:single-signature - pkh, wpkh(native segwit), sh(wpkh, wrapped segwit)
  ;;                  :multi-signature - multi(n), wsh(multi(n)
  ;;
  )

(defrecord KeyStore [label xpub root-fingerprint derivation
                     ;; last-height addresses tx-cache verified-tx
                     ]
  ;; {:label "seedsigner"
  ;;  :xpub "zpub..."
  ;;  :root-fingerprint "xxx"
  ;;  :derivation "m/84'/0'/0'"}
  )

(defrecord SWWallet [keystore wallet-type encrypt?
                     addresses]
  ;; {:keystore {}
  ;;  :wallet-type #{:single-sig :multi-sig}
  ;;  :encrypted #{true false}}
  ;;
  ;;  SWWalletAPI
  )


(def scannecd-ur {:master-fingerprint "7c53ab6b", ;; master fingerprint of the root public key
                  :output-descriptors             ;; how to derive keys and construct addresses
                  [{:hd-key
                    {:chain-code
                     "f62bc1cb5bda8e16fb2febbf45213be6f65e2a60efe4e9e2fdce13a932d76d84",
                     :parent-fingerprint "4715e00f", ;; where this key is derived from
                     :key "0248fbc77e20cbd1b0bddda9ff7c7d34d76102dec42454f040346441e0a203b3d8",
                     :origin ;; details of the key's origin
                     {:source-fingerprint "7c53ab6b",
                      :depth 3,
                      :path "84'/0'/0'",
                      :components [{:index 84} {:index 0} {:index 0}]}},
                    :script-expressions ;; script type
                    [{:tag-value 404,
                      :expression "wpkh",
                      :declaring-class
                      com.sparrowwallet.hummingbird.registry.ScriptExpression}]}]})

(defn ur-account->wallet [{:keys [root-fingerprint chain-code key parent-fingerprint
                                  depth path script] :as ur-account}]
  ;; Only support 84'(wpkh) and 48'(wsh)
  ;; FIXME: cleanup fn args bytes vs integer, etc

  ;; version: path 84 == single-sig, 48 == multi-sig
  ;;          xpub is for inglegacy(P2PKH) and wrapped SegWit(P2SH-P2WPKH)
  (let [key-bytes (codecs/hex->bytes key)
        chain-code-bytes (codecs/hex->bytes chain-code)
        fingerprint-bytes (codecs/hex->bytes parent-fingerprint)
        child-index (-> path b44/parse-path b44/path->vector last)
        xpub (ecc/make-public-key {:key key-bytes
                                   :chain-code chain-code-bytes
                                   :version (get-in net/+networks+ ["main" "xpub"])
                                   :fingerprint fingerprint-bytes
                                   :depth depth
                                   :child-index child-index})
        Zpub (ecc/make-public-key {:key key-bytes
                                   :chain-code chain-code-bytes
                                   :version (get-in net/+networks+ ["main" "Zpub"])
                                   :fingerprint fingerprint-bytes
                                   :depth depth
                                   :child-index child-index})]
    (map->KeyStore {:label "fixme"
                    :xpub {:xpub (b32/encode-hd-key xpub)
                           :Zpub (b32/encode-hd-key Zpub)}
                    :root-fingerprint root-fingerprint
                    :derivation path})))
#_
(ur-account->wallet {:root-fingerprint "431e99fd",
                     :chain-code
                     "0a6d300fe01b0f5f8ffc59f3aa7132453d374cf806c9610319242bbf43dc7ba7",
                     :parent-fingerprint "376754e8",
                     :key "0305db93c49e311ada73fd58611b4cf76340a3223cbce4ac42f6db924d5f73c5b4",
                     :depth 3,
                     :path "m/84'/0'/0'",
                     :script "wpkh"})
