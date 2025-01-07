(ns bitclojr.script
  (:require [bitclojr.base58 :as b58]
            [bitclojr.networks :as net]
            [bitclojr.bip32 :as b32]
            [bitclojr.segwit-addr :as segaddr]
            [bitclojr.util :as util]
            [bitclojr.base58 :as b58]
            [bitclojr.ecc :as ecc]
            [bitclojr.bip341 :as bip341]
            [buddy.core.hash :as hash]
            [clojure.string :as str]
            ;; [bitclojr.bip39 :as b39]
            ;; [buddy.core.crypto :as crypto]
            ;; [buddy.core.mac :as mac]
            [buddy.core.codecs :as codecs]
            ))

(defprotocol NonBech32
  (network-key [this])
  (data-start-index [this])
  (data-end-index [this]))

(defrecord P2PKH [data]
  NonBech32
  (network-key [this]
    "p2pkh")
  (data-start-index [this]
    3)
  (data-end-index [this]
    23))

(defn p2pkh [privkey]
  (->P2PKH `[0x76 0xa9 0x14
             ~@(-> privkey
                   b32/private-key->public-key
                   :key
                   util/hash160)
             0x88 0xac]))

;; (p2pkh (ecc/make-public-key {:key (byte-array (repeat 32 0x11))}))

(defrecord P2SH [data]
  NonBech32
  (network-key [this]
    "p2sh")
  (data-start-index [this]
    2)
  (data-end-index [this]
    22))

(defrecord P2WPKH [data])
(defrecord P2WSH [data])
(defrecord P2TR [data])

(defn address [p2-record]
  (let [data (:data p2-record)]
    (cond (or (instance? P2PKH p2-record) (instance? P2SH p2-record))
          (b58/encode-check (byte-array `[~@(get-in net/+networks+ ["main" (network-key p2-record)])
                                          ~@(subvec data
                                                    (data-start-index p2-record)
                                                    (data-end-index p2-record))])
                            :bytes)
          (or (instance? P2WPKH p2-record) (instance? P2WSH p2-record) (instance? P2TR p2-record))
          (let [op-n (first data)
                version (cond (zero? op-n) 0
                              (<= 0x51 op-n 0x60) (- op-n 0x50)
                              :else (throw (ex-info "Invalid OP-n for witness version n"
                                                    {:version op-n})))]
            (segaddr/encode (get-in net/+networks+ ["main" "bech32"])
                            version
                            (subvec data 2)))
          :else (throw (ex-info "Invalid record type" {:data p2-record})))))

(def +mainnet-key->prefix+
  (into {} (map (fn [[k script-prefix script-postfix]]
                  `[~@(get-in net/+networks+ ["main" k])
                    [~script-prefix ~script-postfix]])
                [["p2pkh" [0x76 0xa9 0x14] [0x88 0xac]]
                 ["p2sh" [0xa9 0x14] [0x87]]])))

(defonce +segwit-address-constraints+
  {:p2wpkh {:ver 0 :size 20}
   :p2wsh {:ver 0 :size 32}
   :p2tr {:ver 1 :size 32}})

(defn- ver+data->segwit-type [version data]
  (-> (for [[k {:keys [ver size]}] +segwit-address-constraints+
            :when (and (= ver version) (= size (count data)))]
        k)
      first))

(defn address->script-pubkey [addr]
  (try (let [data (b58/decode-check addr :bytes)
             [script-prefix script-postfix]
             (get +mainnet-key->prefix+ (get data 0))]
         `[~@script-prefix ~@(rest data) ~@script-postfix])
       (catch Exception _
         ;;Try Bech32 address
         (let [hrp (-> addr (str/split #"1") first)
               [ver data] (segaddr/decode hrp addr)
               updated-ver (case (ver+data->segwit-type ver data)
                             (:p2wpkh :p2wsh) ver
                             (:p2tr) (+ ver 0x50)
                             :else (throw (ex-info "Invalid BECH32 address"
                                                   {:version ver
                                                    :data data})))]
           `[~updated-ver ~(count data) ~@(map #(util/unsigned->signed %) data)]))))

(defn p2sh [script]
  (->P2SH `[0xa9 0x14 ~@(util/hash160 (:key script)) 0x87]))

;; (p2sh (ecc/make-public-key {:key (byte-array (repeat 32 0x11))}))

(defn p2wpkh [{:keys [key]}]
  (->P2WPKH `[0x00 0x14 ~@(util/hash160 key)]))

;;(p2wpkh (ecc/make-public-key {:key (byte-array (repeat 32 0x11))}))

;;https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#p2wsh
(defn p2wsh [m n keys]
  ;; OP_2 == 82, OP_3 == 83 OP_CHECKMULTISIG == 174
  (->> `[~(+ m 80) ~@(mapcat #(let [k (:key %)]
                                `[~(count k) ~@k])
                             keys)
         ~(+ n 80) 174]
       byte-array
       hash/sha256
       (into [0x00 0x20])
       ->P2WSH))

(defn p2tr [{:keys [key]}]
  (->P2TR `[0x51 0x20 ~@(-> {:key key}
                            ecc/make-public-key
                            bip341/taproot-tweak
                            :key)]))
