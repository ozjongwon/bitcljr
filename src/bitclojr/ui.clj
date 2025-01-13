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

(defn root [{:keys [showing selection]}]
  {:fx/type :stage
   :min-width 1024
   :min-height 768
   :showing showing
   :title "BitCljr"
   :scene {:fx/type :scene
           ;;           :stylesheets [(::css/url style)]
           :root {:fx/type :tab-pane
                  ;;:style-class "section"
                  :tabs [{:fx/type :tab
                          :graphic {:fx/type :label
                                    :text "Key Management"}
                          :closable false
                          :content {:fx/type :grid-pane
                                    :children [{:fx/type :label
                                                :grid-pane/row 0
                                                :grid-pane/column 0
                                                :grid-pane/halignment :right
                                                :grid-pane/valignment :top
                                                :text "Keystore"}
                                               {:fx/type :label
                                                :grid-pane/row 1
                                                :grid-pane/column 1
                                                :text "Label"}
                                               {:fx/type :label
                                                :grid-pane/row 1
                                                :grid-pane/column 2
                                                :text "\t"}
                                               {:fx/type :label
                                                :grid-pane/row 1
                                                :grid-pane/column 3
                                                :text "Name(FIXME)"}
                                               {:fx/type :button
                                                :grid-pane/row 1
                                                :grid-pane/column 4
                                                :text "Edit"}
                                               {:fx/type :label
                                                :grid-pane/row 2
                                                :grid-pane/column 1
                                                :text "Master Fingerprint"}
                                               {:fx/type :label
                                                :grid-pane/row 2
                                                :grid-pane/column 2
                                                :text "\t"}
                                               {:fx/type :label
                                                :grid-pane/row 2
                                                :grid-pane/column 3
                                                :text "Fingerprint(FIXME)"}
                                               {:fx/type :label
                                                :grid-pane/row 3
                                                :grid-pane/column 1
                                                :text "Derivation"}
                                               {:fx/type :label
                                                :grid-pane/row 3
                                                :grid-pane/column 2
                                                :text "\t"}
                                               {:fx/type :label
                                                :grid-pane/row 3
                                                :grid-pane/column 3
                                                :text "Derivation(FIXME)"}
                                               {:fx/type :label
                                                :grid-pane/row 4
                                                :grid-pane/column 1
                                                :text "Pub Key"}
                                               {:fx/type :label
                                                :grid-pane/row 4
                                                :grid-pane/column 2
                                                :text "\t"}
                                               {:fx/type :label
                                                :grid-pane/row 4
                                                :grid-pane/column 3
                                                :text "Pub key(FIXME)"}
                                               ;; {:fx/type :label
                                               ;;  :grid-pane/row 2
                                               ;;  :grid-pane/column 1
                                               ;;  :text "baz"}
                                               ;; {:fx/type :label
                                               ;;  :grid-pane/row 3
                                               ;;  :grid-pane/column 0
                                               ;;  :grid-pane/halignment :right
                                               ;;  :grid-pane/valignment :top
                                               ;;  :text "Message"}
                                               ]}}
                         {:fx/type :tab
                          :graphic {:fx/type :label
                                    :text "Address Management"}
                          :closable false
                          :content {:fx/type :grid-pane
                                    :children [{:fx/type :label
                                                :grid-pane/row 0
                                                :grid-pane/column 0
                                                :grid-pane/halignment :right
                                                :grid-pane/valignment :top
                                                :text "Commit SHA"}
                                               {:fx/type :label
                                                :grid-pane/row 0
                                                :grid-pane/column 1
                                                :grid-pane/valignment :top
                                                :text "foo"}
                                               {:fx/type :label
                                                :grid-pane/row 1
                                                :grid-pane/column 0
                                                :grid-pane/halignment :right
                                                :grid-pane/valignment :top
                                                :text "Author"}
                                               {:fx/type :label
                                                :grid-pane/row 1
                                                :grid-pane/column 1
                                                :text "bar"}
                                               {:fx/type :label
                                                :grid-pane/row 2
                                                :grid-pane/column 0
                                                :grid-pane/halignment :right
                                                :grid-pane/valignment :top
                                                :text "Date"}
                                               {:fx/type :label
                                                :grid-pane/row 2
                                                :grid-pane/column 1
                                                :text "baz"}
                                               {:fx/type :label
                                                :grid-pane/row 3
                                                :grid-pane/column 0
                                                :grid-pane/halignment :right
                                                :grid-pane/valignment :top
                                                :text "Message"}]}}]}}})

(renderer {:fx/type root
           :showing true})
