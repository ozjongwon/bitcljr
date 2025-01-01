(ns bitclojr.messages
  (:require [clojure.string :as str]
            [gloss.core :as gloss]
            [gloss.io :as gio]))

(defonce +message-frames+
  {:version (gloss/compile-frame (gloss/ordered-map  :version :int32-le
                                                     :services :uint64-le
                                                     :timestamp :int64-le
                                                     :addr-recv net-addr
                                                     :addr-from net-addr
                                                     :nonce :uint64-le
                                                     :user-agent varstr
                                                     :start-height :int32-le
                                                     :relay relay))
   }  )
(defcodec version-payload )
