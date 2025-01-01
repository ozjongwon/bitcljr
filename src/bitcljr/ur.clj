;; https://github.com/BlockchainCommons/Research/blob/master/papers/bcr-2020-005-ur.md
(ns bitclojr.ur
  (:require [bitclojr.qr :as qr]
            [camel-snake-kebab.core :as csk]
            ;; [clojure.string :as str]
            ;; [clojure.edn :as edn]
            ;; [medley.core :as mc]
            [clojure.walk :as walk]
            [buddy.core.codecs :as codecs]
            )
  (:import [com.sparrowwallet.hummingbird URDecoder ResultType
            registry.CryptoAccount registry.CryptoHDKey registry.CryptoKeypath]))

;; https://github.com/sparrowwallet/hummingbird

(defonce +keys->constructor+
  {})

(defprotocol UrInterface
  (parse [this]))

(defmacro def-ur [name fields & body]
  `(let [def# (defrecord ~name [~@fields]
                UrInterface
                ~@body)]
     (alter-var-root #'+keys->constructor+
                     #(assoc % ~(set (map keyword fields))
                             ~(symbol (str "map->" name))))
     def#))

(defn ur->clj [ur]
  (walk/postwalk (fn [x]
                   (if (map? x)
                     (if-let [constructor (get +keys->constructor+ (set (keys x)))]
                       (constructor x)
                       x)
                     x))
                 ur))

(def-ur Origin [source-fingerprint depth path components]
  (parse [this]
         {:depth depth :path (str "m/" path)}))

(def-ur HdKey [chain-code parent-fingerprint key origin]
  (parse [this]
         (merge (dissoc this :origin)
                (parse origin))))

(def-ur ScriptExpression [tag-value expression declaring-class]
  (parse [this]
         {:script expression}))

(def-ur OutputDescriptor [hd-key script-expressions]
  (parse [this]
         (assert (= 1 (count script-expressions)))
         (merge (parse hd-key)
                (parse (first script-expressions)))))

(def-ur Account [master-fingerprint output-descriptors]
  (parse [this]
         (assert (= 1 (count output-descriptors)))
         (merge {:root-fingerprint master-fingerprint}
                (parse (first output-descriptors)))))

;; (def ex-scannecd-ur {:master-fingerprint "7c53ab6b", ;; master fingerprint of the root public key
;;                      :output-descriptors             ;; how to derive keys and construct addresses
;;                      [{:hd-key
;;                        {:chain-code
;;                         "f62bc1cb5bda8e16fb2febbf45213be6f65e2a60efe4e9e2fdce13a932d76d84",
;;                         :parent-fingerprint "4715e00f", ;; where this key is derived from
;;                         :key "0248fbc77e20cbd1b0bddda9ff7c7d34d76102dec42454f040346441e0a203b3d8",
;;                         :origin ;; details of the key's origin
;;                         {:source-fingerprint "7c53ab6b",
;;                          :depth 3,
;;                          :path "84'/0'/0'",
;;                          :components [{:index 84} {:index 0} {:index 0}]}},
;;                        :script-expressions ;; script type
;;                        [{:tag-value 404,
;;                          :expression "wpkh",
;;                          :declaring-class
;;                          com.sparrowwallet.hummingbird.registry.ScriptExpression}]}]})

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

(defn ->clj [ur-inst]
  (-> (into {}
            (for [[clj-name getter] (get-ur-public-getters (type ur-inst))
                  :let [v (recursively-process-ur-value (invoke-getter ur-inst getter))]
                  :when v]
              [clj-name v]))
      ur->clj))

(defn recursively-process-ur-value [ur-inst]
  (cond (bytes? ur-inst) (codecs/bytes->hex ur-inst)

        (instance? java.util.AbstractCollection ur-inst)
        (mapv ->clj ur-inst)

        (contains? #{CryptoKeypath CryptoHDKey}
                   (class ur-inst))
        (->clj ur-inst)

        :else ur-inst))

(defn qr->ur []
  (println "Scanning...")
  (let [result (.getResult (qr/scan-qr-code-continuously (URDecoder.)))]
    (if (= (.type result) ResultType/SUCCESS)
      (-> result .ur .decodeFromRegistry ->clj)
      (recur))))
