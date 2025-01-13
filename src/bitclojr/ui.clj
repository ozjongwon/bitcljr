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
                (let [padding 10
                      text-color "#111111"]

                  ;; you can put style settings that you need to access from code at keyword keys in a
                  ;; style map and access them directly in an app

                  {::padding padding
                   ::text-color text-color

                   ;; string key ".root" defines `.root` selector with these rules: `-fx-padding: 10;`

                   ".root" {:-fx-padding padding}
                   ".label" {:-fx-text-fill text-color
                             :-fx-wrap-text true}
                   ".button" {:-fx-text-fill text-color
                              ;; vector values are space-separated
                              :-fx-padding ["4px" "8px"]
                              ;; nested string key defines new selector: `.button:hover`
                              ":hover" {:-fx-text-fill :black}}})))

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
   :grid-pane/halignment :right
   :grid-pane/valignment :top
   :text title})

(defn section-field-name-value [{:keys [name value editable?]} row]
  (cond-> [{:fx/type :label
            :style-class "section-field-name"
            :grid-pane/row row
            :grid-pane/column 1
            :text "name"}
           {:fx/type :label
            :style-class "section-field-separator"
            :grid-pane/row row
            :grid-pane/column 2
            :text "\t"}
           {:fx/type :label
            :style-class "section-field-value"
            :grid-pane/row row
            :grid-pane/column 3
            :text "Name(FIXME)"}]
    editable? (conj {:fx/type :button
                     :grid-pane/row row
                     :grid-pane/column 4
                     :text "Edit"})))

(defn ->sections [sections]
  (loop [[section & more-sections] sections orow 0 oresult []]
    (if section
      (recur more-sections
             (inc orow)
             (into oresult
                   (loop [[field & more-fields] (:fields section) irow (inc orow) iresult [(section-title (:section-title section) orow)]]
                     (if field
                       (recur more-fields (inc irow) (into iresult (section-field-name-value field irow)))
                       iresult))))
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
                                      [{:section-title "Keystore"
                                        :fields [{:name "Name1"
                                                  :value "Value11(FIXME)"}
                                                 {:name "Name12"
                                                  :value "Value12(FIXME)"
                                                  :editable? true}]}
                                       {:section-title "Section2"
                                        :fields [{:name "Name21"
                                                  :value "Value21(FIXME)"}
                                                 {:name "Name22"
                                                  :value "Value22(FIXME)"
                                                  :editable? true}]}])
                         (section-tab "Address Management"
                                      [{:section-title "Receive Addresses"
                                        :fields [{:name "Name1"
                                                  :value "Value11(FIXME)"}
                                                 {:name "Name12"
                                                  :value "Value12(FIXME)"
                                                  :editable? true}]}
                                       {:section-title "Change Addresses"
                                        :fields [{:name "Name21"
                                                  :value "Value21(FIXME)"}
                                                 {:name "Name22"
                                                  :value "Value22(FIXME)"
                                                  :editable? true}]}])]}}})

(renderer {:fx/type root
           :showing true})
