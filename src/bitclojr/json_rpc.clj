(ns bitclojr.json-rpc
  (:require [clojure.data.json :as json]
            [clj-http.client :as client])
  (:import [java.time Instant ZoneId format.DateTimeFormatter LocalDateTime])

  ;; (:import [java.net InetSocketAddress Proxy Proxy$Type Socket]
  ;;          [com.github.arteam.simplejsonrpc.client Transport])
  )

(defn parse-timestamp [timestamp]
  (let [instant (Instant/ofEpochSecond timestamp)
        formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")
        zone-id (ZoneId/systemDefault)  ;; Use system default time zone
        local-time (.atZone instant zone-id)]
    (.format local-time formatter)))

(defn parse-date-to-timestamp [date-str]
  (let [formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")
        local-date-time (-> (cond (re-matches #"\d{4}\-\d{2}\-\d{2} \d{2}:\d{2}:\d{2}" date-str)
                                  date-str
                                  (re-matches #"\d{4}\-\d{2}\-\d{2}" date-str)
                                  (str date-str " 00:00:00"))
                            (LocalDateTime/parse formatter))
        zone-id (ZoneId/systemDefault) ;; Use system default time zone
        instant (.atZone local-date-time zone-id)
        timestamp (.toEpochSecond instant)] ;; Convert to Unix timestamp (seconds)
    timestamp))

(defprotocol JsonRpcMedium
  (request [this json]))

(defrecord HttpMedium [endpoint userpw]
  JsonRpcMedium
  (request [this json]
    (try
      (client/post endpoint { ;;socket-timeout 1000      ;; in milliseconds
                             ;;:connection-timeout 1000  ;; in milliseconds
                             :basic-auth userpw
                             :body json
                             ;;:headers {}
                             :content-type :json
                             :accept :json
                             :cookie-policy :none
                             })
      (catch clojure.lang.ExceptionInfo e
        (let [{:keys [error id]} (-> e
                                     ex-data
                                     :body
                                     json/read-json)]
          (->> id
               (assoc (dissoc error :message) :id)
               (ex-info (:message error))
               throw))))))

(defn make-http-medium [endpoint user pw]
  (->HttpMedium endpoint (str user ":" pw)))

(def +http-medium+ (make-http-medium "http://192.168.1.123:8332"
                                     "specter"
                                     "MtldBRBKiJQDEcd"))

(def ^:dynamic *medium* nil)

(defn json-rpc
  ([medium batch-params]
   (->> batch-params
        (map #(assoc % :jsonrpc "2.0" :id (gensym "id")))
        json/write-str
        (request medium)
        :body
        json/read-json
        (map :result)))
  ([medium method params]
   (let [id (gensym "id")]
     (->> {:jsonrpc "2.0" :id id :method method :params params}
          json/write-str
          (request medium)
          :body
          json/read-json
          :result))))

(defonce +genesis-block-info+ {:hash "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f"
                               :time 1231006505
                               :height 0
                               :merkleroot "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b"
                               :tx ["4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b"]})

(defn guess-num-blocks-a-day
  ([current-height]
   (guess-num-blocks-a-day (:time +genesis-block-info+) (quot (System/currentTimeMillis) 1000) current-height))
  ([t0 t1 current-height]
   (quot current-height (quot (- t1 t0) (* 24 60 60)))))

(defn approximate-height
  ([target-t {:keys [time height]}]
   (approximate-height target-t (:time +genesis-block-info+) 0 time height))
  ([target-t start-time start-height end-time end-height]
   (->> start-height
        (- end-height)
        (* (/ (- target-t start-time)
              (- end-time start-time)))
        (+ start-height)
        int)))

(defn find-start-block [medium start-date-str current-height]
  (letfn [(height->block [height]
            (->> [(json-rpc medium "getblockhash" [height])]
                 (json-rpc medium "getblock")) )]
    (let [target-t (parse-date-to-timestamp start-date-str)]
      (loop [start-height (approximate-height target-t (height->block current-height))
             end-height (min current-height (int (* start-height 1.05)))]
        (let [{:keys [time height] :as block} (-> start-height
                                                  (+ end-height)
                                                  (quot 2)
                                                  height->block)
              time-diff (- target-t time)]
          (cond (or (<= (abs time-diff) 60) ;; 10 min
                    (<= (- end-height start-height) 10)) block
                (pos? time-diff) (recur height end-height)
                :else (recur start-height height)))))))

(defn build-tx-history [medium start-date]
  (let [current-height (json-rpc medium "getblockcount" [])
        {:keys [height] :as start-block} (find-start-block medium start-date current-height)]
    (->> (inc current-height)
         (range height)
         (partition 100)
         (map (fn [heights]
                (map (fn [h]
                       {:method "getblockhash" :params [h]})
                     heights)))
         (map #(json-rpc medium %))
         ;;(#(do (println "***1 " %) %))
         (map (fn [hashes]
                (map (fn [hash]
                       ;;  verbosity 2
                       {:method "getblock" :params [hash 2]})
                     hashes)))
         ;;(#(do (println "***2 " %) %))
         (map #(json-rpc medium %))
         (map :tx)
         )

    ;;start-block


    ))


;; (json-rpc +http-medium+ "getblockcount" [])
(comment
  (json-rpc +http-medium+ "getblockcount" [])
  ;;  {:result 574739, :error nil, :id "id17866"}
  (json-rpc +http-medium+ "getblockhash" [574739])
  ;; {:result "00000000000000000026bcc9480a6a3ae962024f3dc96cd4e042c411a9b83e19",
  ;;  :error nil,
  ;;  :id "id17869"}
  (json-rpc +http-medium+ "getblock" ["00000000000000000026bcc9480a6a3ae962024f3dc96cd4e042c411a9b83e19"])
  ;; {:result
  ;;   {:strippedsize 909493,
  ;;    :hash "00000000000000000026bcc9480a6a3ae962024f3dc96cd4e042c411a9b83e19",
  ;;    :versionHex "2000e000",
  ;;    :difficulty 6.702169884349173E12,
  ;;    :time 1557082828,
  ;;    :previousblockhash
  ;;    "0000000000000000000a47828d04d89e2a5323c3bb1cd01475befd156abd551c",
  ;;    :nextblockhash
  ;;    "0000000000000000000b11af14da6a7eec5e96d6f06ad359f56954eb2ddb20d9",
  ;;    :merkleroot
  ;;    "cf7c2ed7a0b5d42f863985d3ac20126106fb5256316df40b210ded070bb02e37",
  ;;    :bits "1729ff38",
  ;;    :size 1270004,
  ;;    :confirmations 99,
  ;;    :tx
  ;;    ["830d2d3b63a63de82990211e363e61b39274da062e935f158439b66b2c504974"
  ;;     "574e25e0e74803253e39eeeb2aee25359c94d09a3843b360a79798a0df5a33d2"
  ;;     "c710b9d19e35ab49be7ad35062c4395919798330c4aebb8cb94f63e8f76bf33],
  ;;  :weight 3998483,
  ;;  :chainwork
  ;;  "00000000000000000000000000000000000000000619b83dd4313a4ee63ee98c",
  ;;  :nTx 3137,
  ;;  :version 536928256,
  ;;  :nonce 2166595842,
  ;;  :height 574739,
  ;;  :mediantime 1557076785},
  ;; :error nil,
  ;; :id "id17872"}
  )



(comment
  ;; https://developer.bitcoin.org/reference/rpc/
  ;;
  ;; Yes, Bitcoin Core provides several RPC commands to view peer information:
  ;;
  ;; getpeerinfo
  ;; Most detailed peer information
  ;; Shows all current connections
  ;; Includes: version, ping time, sync status, bytes sent/received
  ;; getnodeaddresses
  ;; Lists known node addresses
  ;; Can specify how many addresses to return
  ;; Example: getnodeaddresses 10
  ;; getaddednodeinfo
  ;; Shows information about manually added nodes
  ;; Only includes nodes added via addnode
  ;; getnetworkinfo
  ;; General network information
  ;; Includes number of connections
  ;; Shows network protocols enabled

  ;; https://github.com/kyuupichan/electrumx/blob/master/docs/protocol-methods.rst#serverpeerssubscribe
  ;; server.peers.subscribe

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
  )
