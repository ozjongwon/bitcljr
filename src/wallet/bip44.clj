(ns wallet.bip44
  (:require [clojure.string :as str]
            [buddy.core.mac :as mac]
            [buddy.core.hash :as hash]
            [wallet.base58 :as b58]
            [wallet.bip39 :as b39]
            [wallet.networks :as net]
            [wallet.ecc :as ecc]
            [wallet.util :as util]
            [buddy.core.codecs :as codecs])
  (:import [org.bouncycastle.crypto.params ECPrivateKeyParameters ECDomainParameters]
           [org.bouncycastle.crypto.ec CustomNamedCurves]))


(defonce +hardened-index+ 0x80000000)

(defn hardened-index? [i]
  (<= +hardened-index+  i 0xffffffff))

(defn harden [x]
  (let [hardened (+ x +hardened-index+)]
    (assert (hardened-index? hardened))
    hardened))

(defprotocol BIP44
  (change-type [this])
  (discover-account [this master-private-key])
  (path->vector [this]))

;; FIXME: don't need??
(defonce +known-coin-types+
  (->> {:btc-mainnet 0 :btc-testnet 1}
       (map (fn [[_ i]]
              (+ i +hardened-index+)))
       set))

(defrecord BIP44Path [purpose coin-type account change address-index]
  BIP44
  (change-type [this]
    (case change
      0 :external
      1 :internal))
  (discover-account [this master-private-key]
    (assert (= purpose 44))
    (assert (contains? +known-coin-types+ coin-type)))
  (path->vector [this]
    (vec (for [i [purpose coin-type account change address-index]
               :when i]
           i))))

(defn parse-path [path]
  ;; "m/44h/1'/0'/0/32" => purpose coin type account change address-index
  (let [[m & path-str] (str/split path #"/")
        _ (assert (= m "m"))
        [purpose coin-type account change address-index]
        (for [p-str path-str
              :let [hardened? (contains? #{\' \h \H} (last p-str))]]
          (if hardened?
            (-> (subs p-str 0 (dec (count p-str))) (Integer/parseInt) (+ +hardened-index+))
            (Integer/parseInt p-str)))]
    (->BIP44Path purpose coin-type account change address-index)))
