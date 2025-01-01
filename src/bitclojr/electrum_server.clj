(defonce +timeouts+ {:slow-read-timeout-secs []
                     :base-read-timeout-secs []})

(defrecord tor-transport [server proxy timeouts socket]
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
