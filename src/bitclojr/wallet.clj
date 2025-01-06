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
                      (b44/harden 84) :single-sig
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

(defn generate-multi-sig-addresses
  ([wallet m n]
   (generate-multi-sig-addresses wallet m n 0 0))
  ([{:keys [keystores] :as wallet} m n receive-address-index change-address-index]
   (let [[receive-parents change-parents] (->> keystores
                                               (map :xpub)
                                               (map b32/decode-hd-key)
                                               (map (fn [pkey]
                                                      [(b32/derive-child pkey 0)
                                                       (b32/derive-child pkey 1)]))
                                               (apply map list))]
     (loop [gap +address-gap+
            receive-idx receive-address-index
            change-idx change-address-index
            result {:receive-addresses []
                    :change-addresses []}]
       (if (pos? gap)
         (recur (dec gap) (inc receive-idx) (inc change-idx)
                (-> result
                    (update :receive-addresses conj (->> receive-parents
                                                         (map #(b32/derive-child % receive-idx))
                                                         (util/sort-arrays :key)
                                                         (script/p2wsh m n)
                                                         script/address))
                    (update :change-addresses conj (->> change-parents
                                                        (map #(b32/derive-child % change-idx))
                                                        (util/sort-arrays :key)
                                                        (script/p2wsh m n)
                                                        script/address))))
         result)))))

(defn generate-addresses [{:keys [wallet-type] :as wallet}]
  (if (= :single-sig wallet-type)
    (generate-single-sig-addresses wallet)
    (let [[m n] (->> (str/split wallet-type #"of") (map Integer/parseInt))]
      (generate-multi-sig-addresses wallet m n))))

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
(def ss-wallet {:label "fixme-label",
                :wallet-type :single-sig,
                :encrypt? false,
                :keystores
                {:label "fixme",
                 :xpub
                 "zpub6rRcbeEEaQLJzvPMwFRgYrYthQjzZEJ56SF9qEqE9anp588FrTyzXUfCFak1PvVS5jx1en6K1vZ7nuPrueg1f3jZmDN6jELHbzEMaZZPFRB",
                 :root-fingerprint "224a5c23",
                 :derivation "m/84'/0'/0'"}})
#_
(def ms-wallet {:label "fixme-label",
                :wallet-type "2of3",
                :encrypt? false,
                :keystores
                [{:label "fixme1",
                  :xpub
                  "Zpub75nw3RWNVPpCF8P9n6T8tBY7Xh5AHekX5qSg6eta9fxWk3o8dM5WaeH1fXmTiJcDccvdcSRdXSNujS9CL57nve2bfMXq8hX5RvDWeaRrvB2",
                  :root-fingerprint "78ede2ce",
                  :derivation "m/48'/0'/0'/2'"}
                 {:label "fixme2",
                  :xpub
                  "Zpub75JHhU2ZXHQhY2dEGhx1jMUQQ7KRD543rvuVMK5eXNyKhtPvQkQFoNaZDrzcvDdPJ6Tt4xog9j9ectayqDJbgEKAoygjU8kjXR4XR9tvHQE",
                  :root-fingerprint "06877e2d",
                  :derivation "m/48'/0'/0'/2'"}
                 {:label "fixme3",
                  :xpub
                  "Zpub74FmDooresneZBcgnwYG2XzU8JXiJyYo4517p6MqZhub4GSzFnrWwAKAdyAJXw6wB2qU7PAre81tKszU29agLLKv2ryCAior3EgjN6HCdtL",
                  :root-fingerprint "92ee5c12",
                  :derivation "m/48'/0'/0'/2'"}
                 ]})
