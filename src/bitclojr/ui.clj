(ns bitclojr.ui
  (:require [cljfx.api :as fx]
            [cljfx.ext.list-view :as fx.ext.list-view]
            [cljfx.ext.tab-pane :as fx.ext.tab-pane]
            [cljfx.ext.table-view :as fx.ext.table-view]
            [cljfx.ext.tree-view :as fx.ext.tree-view])
  (:import [javafx.scene.control Tab TreeItem]))

(def renderer
  (fx/create-renderer))

(defn- with-tab-selection-props [props desc]
  {:fx/type fx.ext.tab-pane/with-selection-props
   :props props
   :desc desc})

(defn tab-pane [{:keys [items selection selection-capabilities]}]
  {:pre [(set? selection-capabilities)]}
  (let [selected-tab-id (-> selection sort first)
        _ (assert selected-tab-id)]
    (let-refs (into {}
                    (map (fn [item]
                           {:pre [(string? item)]}
                           [item
                            (merge
                             {:fx/type :tab
                              :graphic {:fx/type :label
                                        :text item}
                              :id item
                              :closable false}
                             (cond-> {}
                                        ; buggy for :read'ing tabs
                               (:write selection-capabilities)
                               (assoc :content {:fx/type :label
                                                :text item})

                               (not (:write selection-capabilities))
                               (assoc :disable (if selected-tab-id
                                                 (not= item selected-tab-id)
                                                 true))))]))
                    items)
      (with-tab-selection-props
        (cond-> {}
          (:read selection-capabilities) (assoc :selected-item (get-ref selected-tab-id))
          (:write selection-capabilities) (assoc :on-selected-item-changed {:event/type ::select-tab}))
        {:fx/type :tab-pane
         :tabs (map #(-> (get-ref %)
                         (assoc :fx/id %))
                    items)}))))

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
