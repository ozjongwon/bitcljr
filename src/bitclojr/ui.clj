(ns bitclojr.ui
  (:require [cljfx.api :as fx]
            [cljfx.ext.list-view :as fx.ext.list-view]
            [cljfx.ext.tab-pane :as fx.ext.tab-pane]
            [cljfx.ext.table-view :as fx.ext.table-view]
            [cljfx.lifecycle :as lifecycle]
            [cljfx.ext.tree-view :as fx.ext.tree-view]
            [cljfx.css :as css]
            [medley.core :as mc]
            [bitclojr.wallet :as wallet])
  (:import [javafx.scene.input Clipboard ClipboardContent]))

;;;
;;; Style
;;;
(def style
  (css/register ::style
                {".section"{"-tab" {:-fx-text-fill :green
                                    :-fx-font-size 20
                                    :-fx-font-weight :bold}
                            "-title" {:-fx-text-fill :blue
                                      :-fx-font-size 18
                                      :-fx-font-weight :bold}
                            "-field" {"-name" {:-fx-font-size 18}
                                      "-value" {:-fx-font-size 18}}}
                 }))

;;;
;;; Layout, etc
;;;
(def *state (atom {:wallet {:label "fixme-label",
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
                             ]}}))

(declare root)

(def renderer  (fx/create-renderer :middleware (fx/wrap-map-desc assoc :fx/type root :showing true)))

(defn section-title [title row-idx]
  {:fx/type :label
   :style-class "section-title"
   :grid-pane/row row-idx
   :grid-pane/column 0
   :grid-pane/halignment :center ;; or :left :right
   :grid-pane/valignment :center ;; or :baseline :bottom :top
   :text title})

(defn section-field-name-value [{:keys [name value button]} row]
  (cond-> [{:fx/type :label
            :style-class "section-field-name"
            :grid-pane/row row
            :grid-pane/column 1
            :text name}
           {:fx/type :label
            :style-class "section-field-separator"
            :grid-pane/row row
            :grid-pane/column 2
            :text "\t"}
           {:fx/type :label
            :style-class "section-field-value"
            :grid-pane/row row
            :grid-pane/column 3
            :text value}]
    button (conj (let [{:keys [text on-action]} button ]
                   (mc/assoc-some {:fx/type :button
                                   :grid-pane/row row
                                   :grid-pane/column 4
                                   :text text}
                                  :on-action on-action)))))

(defn ->sections [sections]
  (loop [[section & more-sections] sections orow 0 oresult []]
    (if section
      (let [[result row]
            (loop [[field & more-fields] (:fields section)
                   irow (inc orow)
                   iresult [(section-title (:section-title section) orow)]]
              (if field
                (recur more-fields (inc irow) (into iresult (section-field-name-value field irow)))
                [iresult irow]))]
        (recur more-sections
               (inc row)
               (into oresult result)))
      oresult)))

(defn section-tab [tab-title sections]
  (update-in {:fx/type :tab
              :graphic {:fx/type :label
                        :style-class "section-tab"
                        :text tab-title}
              :closable false
              :content {:fx/type :scroll-pane
                        :content {:fx/type :grid-pane
                                  :children []}}}
             [:content :content :children]
             into (->sections sections)))

(defn- wallet-details [{:keys [keystores]}]
  (if (rest keystores)
    ["Mutil" "wpkh"]
    ["Single" "wsh"]))

(defn- keystores->map-list [keystores]
  (map-indexed (fn [idx {:keys [label xpub root-fingerprint derivation]}]
                 (let [xpk (->> (partition 5 xpub)
                                (mapcat (fn [i chars]
                                          (let [r (mod i 3)]
                                            (if (zero? r)
                                              chars
                                              (repeat r \.))))
                                        (range))
                                (apply str))]
                   {:section-title (str "Keystore-" idx)
                    :fields [{:name "Type"
                              :value "Airgapped Wallet"}
                             {:name "Label"
                              :value label
                              :button {:text "Edit"}}
                             {:name "Master Fingerprint"
                              :value root-fingerprint}
                             {:name "Derivation"
                              :value derivation}
                             {:name "Address"
                              :value xpk}]}))
               keystores))

(defn key-management-tab [{:keys [wallet-type keystores]}]
  (let [[policy script] (wallet-details keystores)]
    (section-tab "Key Management"
                 (into [{:section-title "Configuration"
                         :fields [{:name "Policy Type"
                                   :value policy
                                   :button {:text "Edit"}}
                                  {:name "Script Type"
                                   :value wallet-type}
                                  {:name "Script Policy"
                                   :value script}]}]
                       (keystores->map-list keystores)))))

(defn copy-to-clipboard [addr]
  (let [clipboard (Clipboard/getSystemClipboard)
        content (ClipboardContent.)]
    (.putString content addr)
    (.setContent clipboard content)))

(defn address-management-tab [wallet]
  (let [{:keys [receive-addresses change-addresses]}
        (wallet/generate-addresses wallet)]
    (section-tab "Address Management"
                 [{:section-title "Receive Addresses"
                   :fields (map (fn [addr]
                                  {:name ""
                                   :value addr
                                   :button {:text "Copy"
                                            :on-action (fn [_]
                                                         (copy-to-clipboard addr))}})
                                receive-addresses)}
                  {:section-title "Change Addresses"
                   :fields (map (fn [addr]
                                  {:name ""
                                   :value addr
                                   :button {:text "Copy"
                                            :on-action (fn [_]
                                                         (copy-to-clipboard addr))}})
                                change-addresses)}]))  )

(defn root [{:keys [wallet showing]}]
  {:fx/type :stage
   :min-width 1024
   :min-height 768
   :showing showing
   :title "BitCljr"
   :scene {:fx/type :scene
           :stylesheets [(::css/url style)]
           :root {:fx/type :tab-pane
                  ;;                  :style-class "section"
                  :tabs [(key-management-tab wallet)
                         (address-management-tab wallet)]}}})

(fx/mount-renderer *state renderer)
