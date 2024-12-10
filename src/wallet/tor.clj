(ns wallet.tor
  (:require [clojure.core.async :as async]
            [clj-okhttp.core :as http]
            [clojure.data.json :as json]))

;;(defonce protocol (gloss/compile-frame (gloss/delimited-frame ["\n" "\r\n" "\r"] (gloss/string :utf-8))))
(comment
  (defonce protocol (gloss/compile-frame (gloss/delimited-frame ["}"] (gloss/string :utf-8))))

  (defn wrap-duplex-stream
    [protocol stream]
    (let [out (s/stream)]
      (s/connect (s/map #(when % ;; Don't do anything for notification.
                           (io/encode protocol %))
                        out)
                 stream)
      (s/splice out
                (io/decode-stream stream protocol))))

  (defn client
    [host port]
    (d/chain (tcp/client {:host host, :port port :name-resolver :noop})
             #(wrap-duplex-stream protocol %)))

  (defonce electrum-hosts
    (atom ["signet-electrumx.wakiyamap.dev" "blackie.c3-soft.com"
           ;; "blockstream.info"
           "electrum.blockstream.info"
           "testnet.aranguren.org"
           ;; "explorerzydxu5ecjrkwceayqybizmpjjznk5izmitf2modhcusuqlid.onion"
           ;; "testnet.hsmiths.com"
           ;; "testnet.qtornado.com" "tn.not.fyi"
           ]))

  (defn connect-electrum-server []
    (let [host (first @electrum-hosts)]
      ;; rotate
      (swap! electrum-hosts #(conj (subvec % 1) host))
      (try @(client host 50001)
           (catch Exception _
             (->> @electrum-hosts
                  (remove #(= host %))
                  vec
                  (reset! electrum-hosts))))))

  (defn query-electrum-server
    ([method params]
     (let [id (str (gensym "id"))]
       (-> {:method method :params params :id id :jsonrpc "2.0"}
           json/write-str
           (str "\r\n")
           (query-electrum-server (connect-electrum-server) id))))
    ([msg conn id]
     (if @(s/put! conn msg)
       (try (let [result @(s/take! conn)]
              (.close conn)
              result)
            (catch Exception _))
       (recur msg (connect-electrum-server) id))))
  )
;; (defonce boradcast-source
;;   {"blockstream.info"
;;    ["https://blockstream.info" "http://explorerzydxu5ecjrkwceayqybizmpjjznk5izmitf2modhcusuqlid.onion"]

;;    "mempool.space"
;;    ["https://mempool.space" "http://mempoolhqx4isw62xs7abwphsq7ldayuidyx2v2oethdhhj6mlo2r6ad.onion"]

;;    "mempool.emzy.de"
;;    ["https://mempool.emzy.de" "http://mempool4t6mypeemozyterviq3i5de4kpoua65r3qkn5i3kknu5l2cad.onion"]

;;    "mempool.bisq.services"
;;    ["https://mempool.bisq.services" "http://mempoolcutehjtynu4k4rd746acmssvj2vz4jbz4setb72clbpx2dfqd.onion"]
;;    })


;; ;;;
;; ;;;
;; ;;;
;; electrum.blockstream.info
;; electrumx.bitcoiner.social
;; electrum1.cipig.net
;; 1984  dig +short dnsseed.bitcoin.dashjr.org
;; 1985  dig +short dnsseed.bluematt.me
;; 1986  dig +short seed.bitcoinstats.com
;; 1987  dig +short seed.bitcoin.sipa.be
;; 1988  dig +short dnsseed.emzy.de
;; 2000  dig +short electrum.blockstream.info
;; 2001  dig +short electrumx.bitcoiner.social
;; 2002  dig +short electrum1.cipig.net
;; bitcoin.jonasschnelli.ch
;; ;; https://github.com/emmanuelrosa/bitcoin-onion-nodes/blob/master/nodes.txt
;; ;; https://raw.githubusercontent.com/emmanuelrosa/bitcoin-onion-nodes/refs/heads/master/nodes.txt


(defn create-noop-dns-resolver
  "Create a No-Op DNS Resolver that does nothing"
  []
  (reify org.apache.http.conn.DnsResolver
    (resolve [this host]
      (make-array java.net.InetAddress 0))))

(defn make-socks5h-proxied-conn-manager [^String hostname ^Integer port]
  (let [socket-factory #(conn-mgr/socks-proxied-socket hostname port)
        registry (conn-mgr/into-registry
                  {"http" (conn-mgr/PlainGenericSocketFactory socket-factory)
                   "https" (conn-mgr/SSLGenericSocketFactory socket-factory
                                                             (conn-mgr/get-ssl-context {}))})]
    (PoolingHttpClientConnectionManager. registry (create-noop-dns-resolver))))

(comment
  (defn make-socks5h-proxied-conn-manager [^String hostname ^Integer port]
    (let [socket-factory #(conn-mgr/socks-proxied-socket hostname port)
          registry (conn-mgr/into-registry
                    {"http" (conn-mgr/PlainGenericSocketFactory socket-factory)
                     "https" (conn-mgr/SSLGenericSocketFactory socket-factory
                                                               (conn-mgr/get-ssl-context {}))})]
      (PoolingHttpClientConnectionManager. registry (create-noop-dns-resolver))))
  ;; (def tor-net {:proxy-host "127.0.0.1"
  ;;               :proxy-port 9050
  ;;               :socket-timeout (* 1 1000)
  ;;               :connection-timeout (* 1 1000)})

  (def tor-net {:connection-manager
                (make-socks5h-proxied-conn-manager "127.0.0.1" 9050)})

  (client/get "https://check.torproject.org" tor-net)

  (client/post "http://222dcrzbvqyl3btwkmhny5nyysmpnindp2ne5zjgorvtute4igictjqd.onion"
               (into tor-net
                     {:as :auto
                      :coerce :always
                      :content-type :application/json
                      :body (json/write-str {:method "getblockchaininfo" :params [] :id "id1"})}))

  (client/post "http://222dcrzbvqyl3btwkmhny5nyysmpnindp2ne5zjgorvtute4igictjqd.onion"
               {:proxy-host "127.0.0.1"
                :proxy-port 9050
                :as :auto
                :coerce :always
                :content-type :application/json
                :body (json/write-str {:method "getblockchaininfo" :params [] :id "id1"})})

  (import [java.net InetAddress Proxy ProxySelector InetSocketAddress URI Proxy$Type])
  (defn custom-proxy-selector []
    (proxy [ProxySelector] []
      (select [uri]
        (if (= (.getScheme uri) "http") ;; You can check the URI scheme (e.g., http, https)
          [(Proxy. Proxy$Type/SOCKS (InetSocketAddress. "127.0.0.1" 9050))]  ;; SOCKS5 proxy
          [])))  ;; Return an empty list if no proxy is needed for other schemes
    )

  (json/write-str
   {:jsonrpc "2.0"
    :method method
    :params params
    :id (str (java.util.UUID/randomUUID))})
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
