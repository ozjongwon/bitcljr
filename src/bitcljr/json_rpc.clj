(ns bitclojr.json-rpc
  ;; (:require [clojure.core.async :as async]
  ;;           [clojure.data.json :as json]
  ;;           [clojure.java.io :as io]
  ;;           ;;[clj-okhttp.core :as http]
  ;;           )
  (:import [java.net InetSocketAddress Proxy Proxy$Type Socket]
           [com.github.arteam.simplejsonrpc.client Transport]))

(defonce +timeouts+ {:slow-read-timeout-secs []
                     :base-read-timeout-secs []})

(defrecord tor-tcp-transport [server proxy timeouts socket]
  (connect [this ssl?]
    (let [soc (Socket. proxy)]
      (.connect soc server)
      (.setSoTimeout soc 5000)
      (if ssl?
        (make-ssl-socket (assoc this :socket soc))
        :fix))
    (assoc this :socket soc))
  (connected? [this]
    (and socket (not (.isClosed socket))))
  (close [this]
    (.close socket)
    (assoc this :socket nil))
  (read-distribute-loop [this]
    ;;read & put the result in go channel?
    ))


(defn make-tor-transport [server proxy]
;; btc core + onion = slow, otherwise base
)

(defrecord tor-tcp-tls-transport [server proxy timeouts socket]
(connect [this]
         (let [soc (Socket. proxy)]
           (.connect soc server)
           (.setSoTimeout soc 5000)
           (assoc this :socket soc)))
(connected? [this]
            (and socket (not (.isClosed socket))))
(close [this]
       (.close socket)
       (assoc this :socket nil))
(read-distribute-loop [this]
                      ;;read & put the result in go channel?
                      ))


(defn make-tor-transport [server proxy]
;; btc core + onion = slow, otherwise base
)
