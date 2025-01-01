(ns wallet.wallet
  (:require [wallet.bip44 :as b44]
            [wallet.bip32 :as b32]
            [wallet.ecc :as ecc]
            [wallet.networks :as net]
            [wallet.util :as util]
            [buddy.core.codecs :as codecs]
            [clojure.string :as str]
            ;; [wallet.util :as util]
            ;; [wallet.bip39 :as b39]


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

(defn ur-account->wallet [ur-account]
  ;; Only support 84'(wpkh) and 48'(wsh)
  (let [{:keys [master-fingerprint output-descriptors]} ur-account
        ;; NOTE: ur-account is from SeedSigner and it only supports one descriptor
        ;;       for a single-sig.
        _ (assert (= 1 (count output-descriptors)))
        {:keys [hd-key _]} (first output-descriptors)
        _ (assert hd-key)
        {:keys [chain-code parent-fingerprint key origin]} hd-key
        {:keys [_ depth path _]} origin
        ;;{:keys [tag-value expression]} (first script-expressions)
        ]
    ;; FIXME: cleanup fn args bytes vs integer, etc

    ;; version: path 84 == single-sig, 48 == multi-sig
    ;;          xpub is for inglegacy(P2PKH) and wrapped SegWit(P2SH-P2WPKH)
    (let [full-path (str "m/" path)
          key-bytes (codecs/hex->bytes key)
          chain-code-bytes (codecs/hex->bytes chain-code)
          fingerprint-bytes (codecs/hex->bytes parent-fingerprint)
          child-index (last (b44/path->vector (b44/parse-path full-path)))
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
                      :root-fingerprint master-fingerprint
                      :derivation full-path})))
  ;; 84' wpkh
  #_
  {:master-fingerprint "7c53ab6b",
   :output-descriptors
   [{:hd-key
     {:chain-code
      "f62bc1cb5bda8e16fb2febbf45213be6f65e2a60efe4e9e2fdce13a932d76d84",
      :parent-fingerprint "4715e00f",
      :key "0248fbc77e20cbd1b0bddda9ff7c7d34d76102dec42454f040346441e0a203b3d8",
      :origin
      {:source-fingerprint "7c53ab6b",
       :depth 3,
       :path "84'/0'/0'",
       :components [{:index 84} {:index 0} {:index 0}]}},
     :script-expressions
     [{:tag-value 404,
       :expression "wpkh",
       :declaring-class
       com.sparrowwallet.hummingbird.registry.ScriptExpression}]}]}

  ;; multi-sig wsh = 48',
  #_
  {:master-fingerprint "f5e6bdf3",
   :output-descriptors
   [{:hd-key
     {:chain-code
      "9b9a8620e9e35b73fb9c3289385ada9c97c5f9fe5cd6c6bf78cfb26e8d25bcf4",
      :parent-fingerprint "bbc46130",
      :key "030c2055fce55727b7f7274ce4b8cbe6283c093e319e672967b1d57b3db3ca0c42",
      :origin
      {:source-fingerprint "f5e6bdf3",
       :depth 4,
       :path "48'/0'/0'/2'",
       :components [{:index 48} {:index 0} {:index 0} {:index 2}]}},
     :script-expressions
     [{:tag-value 401,
       :expression "wsh",
       :declaring-class
       com.sparrowwallet.hummingbird.registry.ScriptExpression}]}]}

  #_
  {:master-fingerprint "284c4ef5",
   :output-descriptors
   [{:hd-key
     {:chain-code
      "d11e570bf512874dac6237b42f14d23ecfb2165005cfd28cede5b2275a597fa5",
      :parent-fingerprint "f2838034",
      :key "020a408729ae62582f509626dedae511d65b2f628c6e9f9a8148b4e80aa2ad6b2d",
      :origin
      {:source-fingerprint "284c4ef5",
       :depth 4,
       :path "48'/0'/0'/2'",
       :components [{:index 48} {:index 0} {:index 0} {:index 2}]}},
     :script-expressions
     [{:tag-value 401,
       :expression "wsh",
       :declaring-class
       com.sparrowwallet.hummingbird.registry.ScriptExpression}]}]}

  #_
  {:master-fingerprint "4af1516e",
   :output-descriptors
   [{:hd-key
     {:chain-code
      "79363a1111f09281db6951232064dc476fa5d83ed780aa27aca7bbd26248d257",
      :parent-fingerprint "2ade32f9",
      :key "022d1fb551e1f6c066bf645de623d7b6d853d9faecba8543b2d269fedfeeef0623",
      :origin
      {:source-fingerprint "4af1516e",
       :depth 4,
       :path "48'/0'/0'/2'",
       :components [{:index 48} {:index 0} {:index 0} {:index 2}]}},
     :script-expressions
     [{:tag-value 401,
       :expression "wsh",
       :declaring-class
       com.sparrowwallet.hummingbird.registry.ScriptExpression}]}]}


  )
#_
(ur-account->wallet {:master-fingerprint "f5e6bdf3",
                     :output-descriptors
                     [{:hd-key
                       {:chain-code
                        "9b9a8620e9e35b73fb9c3289385ada9c97c5f9fe5cd6c6bf78cfb26e8d25bcf4",
                        :parent-fingerprint "bbc46130",
                        :key "030c2055fce55727b7f7274ce4b8cbe6283c093e319e672967b1d57b3db3ca0c42",
                        :origin
                        {:source-fingerprint "f5e6bdf3",
                         :depth 4,
                         :path "48'/0'/0'/2'",
                         :components [{:index 48} {:index 0} {:index 0} {:index 2}]}},
                       :script-expressions
                       [{:tag-value 401,
                         :expression "wsh",
                         :declaring-class
                         com.sparrowwallet.hummingbird.registry.ScriptExpression}]}]})

(let [chain-code (codecs/hex->bytes "9b9a8620e9e35b73fb9c3289385ada9c97c5f9fe5cd6c6bf78cfb26e8d25bcf4"),
      key (codecs/hex->bytes "030c2055fce55727b7f7274ce4b8cbe6283c093e319e672967b1d57b3db3ca0c42")
      fingerprint (codecs/hex->bytes "bbc46130") ;; parent fingerprint
      path (b44/path->vector (b44/parse-path "m/48'/0'/0'/2'"))
      depth (count path)
      child-index (last path)
      _ (println "***" path depth child-index)
      xpub (ecc/make-public-key {:key key
                                 :chain-code chain-code
                                 :version (get-in net/+networks+ ["main" "xpub"])
                                 :fingerprint  fingerprint
                                 :depth depth
                                 :child-index child-index})
      zpub (ecc/make-public-key {:key key
                                 :chain-code chain-code
                                 :version (get-in net/+networks+ ["main" "zpub"])
                                 :fingerprint fingerprint
                                 :depth depth
                                 :child-index child-index})]
  (println "****" (codecs/bytes->hex fingerprint))
  {:xpub (b32/encode-hd-key xpub)
   :zpub (b32/encode-hd-key zpub)})
(ur-account->wallet {:master-fingerprint "f5e6bdf3",
                     :output-descriptors
                     [{:hd-key
                       {:chain-code

                        :parent-fingerprint "bbc46130",
                        :key ,
                        :origin
                        {:source-fingerprint "f5e6bdf3",
                         :depth 4,
                         :path "48'/0'/0'/2'",
                         :components [{:index 48} {:index 0} {:index 0} {:index 2}]}},
                       :script-expressions
                       [{:tag-value 401,
                         :expression "wsh",
                         :declaring-class
                         com.sparrowwallet.hummingbird.registry.ScriptExpression}]}]})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol HWWalletAPI
  (xpub [this]
    ;; return master public key for SW wallet
    )

  (derive-child-key [this derivation-path]
    ;; for private keys to sign
    )

  (tx-details [this tx]
    ;; before signing details
    )
  (sign-tx [this tx])

  (get-info [this]
    ;; model, version, etc
    ))
;;;
;;; the master private key -> get-xpub -> recieve addresses
;;;

#_
(defprotocol SWWalletAPI
  SWWalletAPI
  (xpub [this]
    ;; return master public key in the SW wallet
    )

  (derive-child-key [this derivation-path]
    ;; receive addreses
    )

  (generate-address [this address-type]
    ;; :p2wpkh p2tr m/48/0/0/0 or m/48/0/0/1
    )

  (create-tx-proposal [this inputs outputs]
    ;; return  unsigned tx
    )

  (broadcast-tx [this signed-transaction]
    ;;return tx hash(txid)
    )
  (get-info [this]
    ;; balance, pub keys(receive & utxo), # used addresses, etc
    )

  (get-master-fingerprint [this]
    ;; xpub
    )

  (get-balance [this]
    ;; xpub
    )

  (get-transaction-history [this]
    ;; txid, amount, time
    ))

#_
(defrecord HWWallet [name master-private-key]
  WalletAPI
  (generate-key-pair [this derivation-path]
    ;; using derivation path
    )
  (derive-child-key [this derivation-path]
    ;; 32/44/84
    )
  (get-receive-address [this derivation-path]
    ;; 32/44/84
    )
  )

(comment

;;;;
;;;;

;;;;;;;;;;;;;;;;;;;;
  ;; 1. Seed + Passphrase from storage (encrypted)
  ;; 2. Derive xpub & master fingerprint

  (def seed (-> "rice doll mixture lobster direct orange fabric merge canyon cereal either review"
                (b39/mnemonic->seed "Hello World!")))

  (def qr-scan-cbor "LPCSECAACSJKCYNDIDCYTLHDCABSTPLRKOVOSAJEIHBTZEDEEYPARPAMBSGEBZFHLBCXBAWDIYRHKGSAFYLFMEBEKBBG")

  ;;"UR:CRYPTO-ACCOUNT/53-4/LPCSECAACSJKCYNDIDCYTLHDCABSTPLRKOVOSAJEIHBTZEDEEYPARPAMBSGEBZFHLBCXBAWDIYRHKGSAFYLFMEBEKBBG"

  ;; **** CRYPTO-ACCOUNT -> ACCOUNT-UR =>>> URDecoder
  ;;      CRYPTO-PSBT -> PSBT-UR2      =>>> URDecoder
  ;;

  "p(\\d+)of(\\d+) (.+)"
  (b32/decode "LPCSECAACSJKCYNDIDCYTLHDCABSTPLRKOVOSAJEIHBTZEDEEYPARPAMBSGEBZFHLBCXBAWDIYRHKGSAFYLFMEBEKBBG")


  (-> "rice doll mixture lobster direct orange fabric merge canyon cereal either review"
      (b39/mnemonic->seed "Hello World!")
      codecs/hex->bytes
      b32/seed->hd-key
      b32/ec-key->fingerprint
      codecs/bytes->hex
      )

  (-> "rice doll mixture lobster direct orange fabric merge canyon cereal either review"
      (b39/mnemonic->seed "Hello World!")
      codecs/hex->bytes
      b32/seed->hd-key
      (b32/path->child "m/84'/0'/0'") ;; P2WPKH == 84
      (b32/private-key->public-key)
      b32/encode-hd-key
      (= "xpub6CJTXjz9EUoar7X28ZnYayRQrQzVnMoiE8KisZHbKPcugTaU3DKpLqj7oQCeaBUDoTqFjcVoDFih9N84WKzocwapHiGdRN3bp4ZV3ywHHy8"))

  )
