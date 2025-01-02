(ns bitclojr.wallet
  (:require [bitclojr.bip44 :as b44]
            [bitclojr.bip32 :as b32]
            [bitclojr.ecc :as ecc]
            [bitclojr.networks :as net]
            [bitclojr.util :as util]
            [bitclojr.slip-0132 :as s0132]
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
        xpub (-> {:key key-bytes
                  :chain-code chain-code-bytes
                  :fingerprint fingerprint-bytes
                  :depth depth
                  :child-index child-index}
                 (assoc :version (get-in net/+networks+
                                         ["main" (s0132/get-address-types ur-account)]))
                 (ecc/make-public-key))]
    (-> {:label "fixme"
         :xpub (b32/encode-hd-key xpub)
         :root-fingerprint root-fingerprint
         :derivation path}
        (map->KeyStore))))
#_
(def ss-ex
  {:root-fingerprint "78ede2ce",
   :chain-code
   "bc9473195e203536a962fa211c02a5929effb597bb7a946de48067297b29042c",
   :parent-fingerprint "0e144480",
   :key "03e0b0bb752a26ee43a2733b3fdbc6632379d5e23d20f9b3b6cd8d82ca4ec72c7c",
   :depth 3,
   :path "m/84'/0'/0'",
   :script "wpkh"})
#_
(def ms-ex {:root-fingerprint "78ede2ce",
            :chain-code
            "aeb17b220446b72ac204b677067f3a21e2a3ba9683a5afcb229d5a82cb96ab02",
            :parent-fingerprint "e6d8998e",
            :key "0244432d006c357920078df950e5a1114756c925cfd07f7c1352f20c49fabf7b45",
            :depth 4,
            :path "m/48'/0'/0'/2'",
            :script "wsh"})
