(ns bitclojr.ui
  (:require [cljfx.api :as fx]
            [cljfx.ext.list-view :as fx.ext.list-view]
            [cljfx.ext.tab-pane :as fx.ext.tab-pane]
            [cljfx.ext.table-view :as fx.ext.table-view]
            [cljfx.ext.tree-view :as fx.ext.tree-view])
  (:import [javafx.scene.control Tab TreeItem]))

(def renderer
  (fx/create-renderer))

(defn add-header [title desc]
  {:fx/type :v-box
   :spacing 10
   :children [{:fx/type :label
               :text title}
              desc]})

(defn root [{:keys [showing selection]}]
  {:fx/type :stage
   :min-width 1024
   :min-height 768
   :showing showing
   :title "BitCljr"
   :scene {:fx/type :scene
           :root {:fx/type :tab-pane
                  :tabs [{:fx/type :tab
                          :graphic {:fx/type :label
                                    :text "Key Management"}}
                         {:fx/type :tab
                          :graphic {:fx/type :label
                                    :text "Address Management"}}]}}})

(renderer {:fx/type root
           :showing true})
