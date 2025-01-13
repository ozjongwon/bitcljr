(ns bitclojr.ui
  (:require [cljfx.api :as fx]
            [cljfx.ext.list-view :as fx.ext.list-view]
            [cljfx.ext.tab-pane :as fx.ext.tab-pane]
            [cljfx.ext.table-view :as fx.ext.table-view]
            [cljfx.lifecycle :as lifecycle]
            [cljfx.ext.tree-view :as fx.ext.tree-view]
            [cljfx.css :as css]
            [medley.core :as mc])
  ;;(:import [javafx.scene.control Tab TreeItem])
  )

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
(def renderer
  (fx/create-renderer))

(defn section-title [title row-idx]
  {:fx/type :label
   :style-class "section-title"
   :grid-pane/row row-idx
   :grid-pane/column 0
   :grid-pane/halignment :center ;; or :left :right
   :grid-pane/valignment :center ;; or :baseline :bottom :top
   :text title})

(defn section-field-name-value [{:keys [name value editable?]} row]
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
    editable? (conj {:fx/type :button
                     :grid-pane/row row
                     :grid-pane/column 4
                     :text "Edit"})))

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
              :content {:fx/type :grid-pane
                        :children []}}
             [:content :children]
             into (->sections sections)))

(defn root [{:keys [showing selection]}]
  {:fx/type :stage
   :min-width 1024
   :min-height 768
   :showing showing
   :title "BitCljr"
   :scene {:fx/type :scene
           :stylesheets [(::css/url style)]
           :root {:fx/type :tab-pane
                  ;;                  :style-class "section"
                  :tabs [(section-tab "Key Management"
                                      [{:section-title "Configuration"
                                        :fields [{:name "Policy Type"
                                                  :value "Single or Multi"
                                                  :editable? true}
                                                 {:name "Script Type"
                                                  :value "Value22(FIXME)"}
                                                 {:name "Script Policy"
                                                  :value "wpkh or wsh"}]}
                                       {:section-title "Keystore"
                                        :fields [{:name "Type"
                                                  :value "Airgapped Wallet"}
                                                 {:name "Label"
                                                  :value "BitClojr"
                                                  :editable? true}
                                                 {:name "Master Fingerprint"
                                                  :value "0x001122ff"}
                                                 {:name "Derivation"
                                                  :value "m/48'/0'/0'/2'"}
                                                 {:name "Address"
                                                  :value "zpub..."}]}])
                         (section-tab "Address Management"
                                      [{:section-title "Receive Addresses"
                                        :fields [{:name ""
                                                  :value "bc1...."}
                                                 {:name ""
                                                  :value "bc1..."}]}
                                       {:section-title "Change Addresses"
                                        :fields [{:name ""
                                                  :value "bc1...."}
                                                 {:name ""
                                                  :value "bc1..."}]}])]}}})

(renderer {:fx/type root
           :showing true})
