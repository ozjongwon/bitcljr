(ns bitclojr.wallet
  (:require [bitclojr.bip44 :as b44]
            [bitclojr.bip32 :as b32]
            [bitclojr.ecc :as ecc]
            [bitclojr.networks :as net]
            [bitclojr.util :as util]
            [bitclojr.slip-0132 :as s0132]
            [bitclojr.script :as script]
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

(defrecord SWWallet [keystores wallet-type encrypt?]
  ;; {:keystore {}
  ;;  :wallet-type #{:single-sig :multi-sig}
  ;;  :encrypted #{true false}}
  ;;
  ;;  SWWalletAPI
  )

(defn make-key-store [root-fingerprint chain-code key parent-fingerprint
                      depth path child-index address-type]
  ;;[key chain-code root-fingerprint fingerprint depth child-index address-type]
  (let [key-bytes (codecs/hex->bytes key)
        chain-code-bytes (codecs/hex->bytes chain-code)
        fingerprint-bytes (codecs/hex->bytes parent-fingerprint)
        xpub (-> {:key key-bytes
                  :chain-code chain-code-bytes
                  :fingerprint fingerprint-bytes
                  :depth depth
                  :child-index child-index}
                 (assoc :version (get-in net/+networks+ ["main" address-type]))
                 (ecc/make-public-key))]
    (-> {:label "fixme"
         :xpub (b32/encode-hd-key xpub)
         :root-fingerprint root-fingerprint
         :derivation path}
        (map->KeyStore))))

(defn ur-account->wallet [{:keys [root-fingerprint chain-code key parent-fingerprint
                                  depth path script] :as ur-account}]
  ;; 84'(wpkh) : single-signed
  ;; 48'(wsh) : multi-signed
  (let [path-record (b44/parse-path path)
        wallet-type (condp = (:purpose path-record)
                      (b44/harden 84) :signle-sig
                      (b44/harden 48) :multi-sig
                      (throw (ex-info "Invalid path" {:path path})))]
    (->> (cond->> (make-key-store root-fingerprint chain-code key parent-fingerprint
                                  depth path
                                  (-> path-record b44/path->vector last)
                                  (s0132/get-address-type ur-account))
           (= wallet-type :multi-sig) (conj []))
         (assoc {:label "fixme-label"
                 :wallet-type wallet-type
                 ;; FIXME
                 :encrypt? false}
                :keystores))))

;; "m/48'/0'/0'/2'"
;; =>
;; receive address
;; m/48'/0'/0'/2'/0/0
;; m/48'/0'/0'/2'/0/1
;;
;; change address
;; m/48'/0'/0'/2'/1/0
;; m/48'/0'/0'/2'/1/1

(defonce +address-gap+ 20)

(defn generate-single-sig-addresses
  ([keystores]
   (generate-single-sig-addresses keystores 0 0))
  ([{{:keys [xpub derivation]} :keystores} receive-address-index change-address-index]
   (let [pkey (b32/decode-hd-key xpub)
         receive-parent (b32/derive-child pkey 0)
         change-parent (b32/derive-child pkey 1)]
     (loop [gap +address-gap+
            receive-idx receive-address-index
            change-idx change-address-index
            result {:receive-addresses []
                    :change-addresses []}]
       (if (pos? gap)
         (recur (dec gap) (inc receive-idx) (inc change-idx)
                (-> result
                    (update :receive-addresses conj (->> receive-idx
                                                         (b32/derive-child receive-parent)
                                                         script/p2wpkh
                                                         script/address))
                    (update :change-addresses conj (->> change-idx
                                                        (b32/derive-child change-parent)
                                                        script/p2wpkh
                                                        script/address))))
         result)))))

#_
(generate-single-sig-addresses {:keystores {:xpub "zpub6rRcbeEEaQLJzvPMwFRgYrYthQjzZEJ56SF9qEqE9anp588FrTyzXUfCFak1PvVS5jx1en6K1vZ7nuPrueg1f3jZmDN6jELHbzEMaZZPFRB" :derivation "m/84'/0'/0'"}})

(defn generate-multi-sig-addresses [{:keys [keystores] :as wallet}]
  (mapv (fn [{:keys [xpub]}]
          (let [pkey (b32/decode-hd-key xpub)]
            pkey))
        keystores))

(defn generate-addresses [{:keys [wallet-type] :as wallet}]
  (case wallet-type
    :single-sig (generate-single-sig-addresses wallet)
    :multi-sig (generate-multi-sig-addresses wallet)))

#_
(def ss-ex
  {:root-fingerprint "224a5c23",
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
