;; https://github.com/BlockchainCommons/Research/blob/master/papers/bcr-2020-005-ur.md
(ns wallet.ur
  (:require [wallet.qr :as qr]
            [camel-snake-kebab.core :as csk]
            ;; [clojure.string :as str]
            ;; [clojure.edn :as edn]
            ;; [medley.core :as mc]
            ;; [clj-cbor.core :as cbor]
            [buddy.core.codecs :as codecs]
            )
  (:import [com.sparrowwallet.hummingbird URDecoder ResultType
            registry.CryptoAccount registry.CryptoHDKey registry.CryptoKeypath]))

;; https://github.com/sparrowwallet/hummingbird

(defn get-ur-public-getters [cls]
  (letfn [(clj-name [m]
            (let [full-name (csk/->kebab-case-string (.getName m))]
              (assert (= "get-" (subs full-name 0 4)))
              (keyword (subs full-name 4))))]
    (->>  cls
          .getMethods
          seq
          (filter #(and (.getName %) ;; Ensure the method has a name
                        (.startsWith (.getName %) "get") ;; Getter methods start with "get"
                        (not= (.getName %) "getRegistryType")
                        (not= "getClass" (.getName %))) ;; Skip getClass method
                  )
          (mapv #(vector (clj-name %) %)))))

(defn invoke-getter [obj getter]
  (.invoke getter obj nil))

(defn recursively-process-ur-value [ur-inst]
  (cond (bytes? ur-inst) (codecs/bytes->hex ur-inst)
        (instance? java.util.AbstractCollection ur-inst)
        (into {} (mapcat ->clj ur-inst))

        (contains? #{CryptoKeypath CryptoHDKey}
                   (class ur-inst))
        (->clj ur-inst)

        :else ur-inst))

(defn ->clj [ur-inst]
  (into {}
        (for [[clj-name getter] (get-ur-public-getters (type ur-inst))
              :let [v (recursively-process-ur-value (invoke-getter ur-inst getter))]
              :when v]
          [clj-name v])))

(defn qr->ur []
  (let [result (.getResult (qr/scan-qr-code-continuously (URDecoder.)))]
    (if (= (.type result) ResultType/SUCCESS)
      (-> result .ur .decodeFromRegistry ->clj)
      (throw (ex-info "qr->ur faile!" {:result-type (.type result)})))))
