;; https://github.com/spesmilo/electrum/blob/master/electrum/servers.json

(ns bitclojr.tor
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.async :as async]
            [flatland.ordered.map :refer [ordered-map]])
  (:import [java.net InetSocketAddress Proxy Proxy$Type Socket]
           [javax.net.ssl SSLContext SSLSocketFactory SSLSocket TrustManager X509TrustManager
            HandshakeCompletedListener SSLSocket]
           [java.security.cert X509Certificate]
           [java.io BufferedWriter PrintWriter BufferedReader InputStreamReader OutputStreamWriter]
           [java.security SecureRandom]
           [java.nio ByteBuffer]
           [java.nio.charset StandardCharsets]
           [java.nio.channels Selector SelectionKey SelectableChannel SocketChannel]))

;;;
;;; Electrum Server, rotating
;;;
(defrecord ElectrumServer [name s])

(defn make-electrum-server [name s]
  (->ElectrumServer name s))

(defonce +electrum-servers+
  (atom (->> {
              "104.248.139.211" {
                                 "pruning" "-",
                                 "s" "50002",
                                 "t" "50001",
                                 "version" "1.4.2"
                                 },
              "128.0.190.26" {
                              "pruning" "-",
                              "s" "50002",
                              "version" "1.4.2"
                              },
              "142.93.6.38" {
                             "pruning" "-",
                             "s" "50002",
                             "t" "50001",
                             "version" "1.4.2"
                             },
              "157.245.172.236" {
                                 "pruning" "-",
                                 "s" "50002",
                                 "t" "50001",
                                 "version" "1.4.2"
                                 },
              "159.65.53.177" {
                               "pruning" "-",
                               "t" "50001",
                               "version" "1.4.2"
                               },
              "167.172.42.31" {
                               "pruning" "-",
                               "s" "50002",
                               "t" "50001",
                               "version" "1.4.2"
                               },
              "188.230.155.0" {
                               "pruning" "-",
                               "s" "50002",
                               "t" "50001",
                               "version" "1.4.2"
                               },
              "22mgr2fndslabzvx4sj7ialugn2jv3cfqjb3dnj67a6vnrkp7g4l37ad.onion" {
                                                                                "pruning" "-",
                                                                                "t" "50001",
                                                                                "version" "1.4.2"
                                                                                },
              "2AZZARITA.hopto.org" {
                                     "pruning" "-",
                                     "s" "50002",
                                     "t" "50001",
                                     "version" "1.4.2"
                                     },
              "2electrumx.hopto.me" {
                                     "pruning" "-",
                                     "s" "56022",
                                     "t" "56021",
                                     "version" "1.4.2"
                                     },
              "2ex.digitaleveryware.com" {
                                          "pruning" "-",
                                          "s" "50002",
                                          "version" "1.4.2"
                                          },
              "37.205.9.165" {
                              "pruning" "-",
                              "s" "50002",
                              "version" "1.4.2"
                              },
              "68.183.188.105" {
                                "pruning" "-",
                                "s" "50002",
                                "t" "50001",
                                "version" "1.4.2"
                                },
              "73.92.198.54" {
                              "pruning" "-",
                              "s" "50002",
                              "version" "1.4.2"
                              },
              "89.248.168.53" {
                               "pruning" "-",
                               "s" "50002",
                               "t" "50001",
                               "version" "1.4.2"
                               },
              "E-X.not.fyi" {
                             "pruning" "-",
                             "s" "50002",
                             "t" "50001",
                             "version" "1.4"
                             },
              "VPS.hsmiths.com" {
                                 "pruning" "-",
                                 "s" "50002",
                                 "t" "50001",
                                 "version" "1.4"
                                 },
              "alviss.coinjoined.com" {
                                       "pruning" "-",
                                       "s" "50002",
                                       "t" "50001",
                                       "version" "1.4.2"
                                       },
              "assuredly.not.fyi" {
                                   "pruning" "-",
                                   "s" "50002",
                                   "version" "1.4.2"
                                   },
              "bejqtnc64qttdempkczylydg7l3ordwugbdar7yqbndck53ukx7wnwad.onion" {
                                                                                "pruning" "-",
                                                                                "s" "50002",
                                                                                "t" "50001",
                                                                                "version" "1.4.5"
                                                                                },
              "bitcoin.aranguren.org" {
                                       "pruning" "-",
                                       "s" "50002",
                                       "t" "50001",
                                       "version" "1.4.2"
                                       },
              "bitcoin.lu.ke" {
                               "pruning" "-",
                               "s" "50002",
                               "t" "50001",
                               "version" "1.4.2"
                               },
              "bitcoins.sk" {
                             "pruning" "-",
                             "s" "56002",
                             "t" "56001",
                             "version" "1.4.2"
                             },
              "blackie.c3-soft.com" {
                                     "pruning" "-",
                                     "s" "57002",
                                     "t" "57001",
                                     "version" "1.4.5"
                                     },
              "blkhub.net" {
                            "pruning" "-",
                            "s" "50002",
                            "version" "1.4.2"
                            },
              "blockstream.info" {
                                  "pruning" "-",
                                  "s" "700",
                                  "t" "110",
                                  "version" "1.4"
                                  },
              "btc.electroncash.dk" {
                                     "pruning" "-",
                                     "s" "60002",
                                     "t" "60001",
                                     "version" "1.4.5"
                                     },
              "btc.litepay.ch" {
                                "pruning" "-",
                                "s" "50002",
                                "version" "1.4.2"
                                },
              "btc.ocf.sh" {
                            "pruning" "-",
                            "s" "50002",
                            "version" "1.4.2"
                            },
              "btce.iiiiiii.biz" {
                                  "pruning" "-",
                                  "s" "50002",
                                  "t" "50001",
                                  "version" "1.4.2"
                                  },
              "de.poiuty.com" {
                               "pruning" "-",
                               "s" "50002",
                               "t" "50004",
                               "version" "1.4.5"
                               },
              "e.keff.org" {
                            "pruning" "-",
                            "s" "50002",
                            "t" "50001",
                            "version" "1.4"
                            },
              "e2.keff.org" {
                             "pruning" "-",
                             "s" "50002",
                             "t" "50001",
                             "version" "1.4"
                             },
              "eai.coincited.net" {
                                   "pruning" "-",
                                   "s" "50002",
                                   "t" "50001",
                                   "version" "1.4.2"
                                   },
              "ecdsa.net" {
                           "pruning" "-",
                           "s" "110",
                           "t" "50001",
                           "version" "1.4"
                           },
              "egyh5mutxwcvwhlvjubf6wytwoq5xxvfb2522ocx77puc6ihmffrh6id.onion" {
                                                                                "pruning" "-",
                                                                                "s" "50002",
                                                                                "t" "50001",
                                                                                "version" "1.4.2"
                                                                                },
              "electrum.bitaroo.net" {
                                      "pruning" "-",
                                      "s" "50002",
                                      "t" "50001",
                                      "version" "1.4.2"
                                      },
              "electrum.blockstream.info" {
                                           "pruning" "-",
                                           "s" "50002",
                                           "t" "50001",
                                           "version" "1.4"
                                           },
              "electrum.dcn.io" {
                                 "pruning" "-",
                                 "s" "50002",
                                 "t" "50001",
                                 "version" "1.4.2"
                                 },
              "electrum.emzy.de" {
                                  "pruning" "-",
                                  "s" "50002",
                                  "version" "1.4.2"
                                  },
              "electrum.hodlister.co" {
                                       "pruning" "-",
                                       "s" "50002",
                                       "version" "1.4"
                                       },
              "electrum.hsmiths.com" {
                                      "pruning" "-",
                                      "s" "50002",
                                      "t" "50001",
                                      "version" "1.4"
                                      },
              "electrum.jochen-hoenicke.de" {
                                             "pruning" "-",
                                             "s" "50006",
                                             "t" "50099",
                                             "version" "1.4.5"
                                             },
              "electrum.pabu.io" {
                                  "pruning" "-",
                                  "s" "50002",
                                  "version" "1.4.2"
                                  },
              "electrum.qtornado.com" {
                                       "pruning" "-",
                                       "s" "50002",
                                       "t" "50001",
                                       "version" "1.4"
                                       },
              "electrum3.hodlister.co" {
                                        "pruning" "-",
                                        "s" "50002",
                                        "version" "1.4"
                                        },
              "electrum5.hodlister.co" {
                                        "pruning" "-",
                                        "s" "50002",
                                        "version" "1.4"
                                        },
              "electrumx.alexridevski.net" {
                                            "pruning" "-",
                                            "s" "50002",
                                            "t" "50001",
                                            "version" "1.4.2"
                                            },
              "electrumx.erbium.eu" {
                                     "pruning" "-",
                                     "s" "50002",
                                     "t" "50001",
                                     "version" "1.4"
                                     },
              "electrumx.schulzemic.net" {
                                          "pruning" "-",
                                          "s" "50002",
                                          "t" "50001",
                                          "version" "1.4.2"
                                          },
              "elx.bitske.com" {
                                "pruning" "-",
                                "s" "50002",
                                "t" "50001",
                                "version" "1.4.2"
                                },
              "ex.btcmp.com" {
                              "pruning" "-",
                              "s" "50002",
                              "version" "1.4.2"
                              },
              "ex03.axalgo.com" {
                                 "pruning" "-",
                                 "s" "50002",
                                 "version" "1.4.2"
                                 },
              "explorerzydxu5ecjrkwceayqybizmpjjznk5izmitf2modhcusuqlid.onion" {
                                                                                "pruning" "-",
                                                                                "t" "110",
                                                                                "version" "1.4"
                                                                                },
              "exs.dyshek.org" {
                                "pruning" "-",
                                "s" "50002",
                                "t" "50001",
                                "version" "1.4.2"
                                },
              "fortress.qtornado.com" {
                                       "pruning" "-",
                                       "s" "443",
                                       "version" "1.5"
                                       },
              "fulcrum.grey.pw" {
                                 "pruning" "-",
                                 "s" "51002",
                                 "t" "51001",
                                 "version" "1.4.5"
                                 },
              "gall.pro" {
                          "pruning" "-",
                          "s" "50002",
                          "version" "1.4.2"
                          },
              "guichet.centure.cc" {
                                    "pruning" "-",
                                    "s" "50002",
                                    "t" "50001",
                                    "version" "1.4.2"
                                    },
              "hodlers.beer" {
                              "pruning" "-",
                              "s" "50002",
                              "version" "1.4.2"
                              },
              "horsey.cryptocowboys.net" {
                                          "pruning" "-",
                                          "s" "50002",
                                          "t" "50001",
                                          "version" "1.4.2"
                                          },
              "kareoke.qoppa.org" {
                                   "pruning" "-",
                                   "s" "50002",
                                   "t" "50001",
                                   "version" "1.4.2"
                                   },
              "kittycp2gatrqhlwpmbczk5rblw62enrpo2rzwtkfrrr27hq435d4vid.onion" {
                                                                                "pruning" "-",
                                                                                "s" "50002",
                                                                                "t" "50001",
                                                                                "version" "1.4.2"
                                                                                },
              "lavahost.org" {
                              "pruning" "-",
                              "s" "50002",
                              "version" "1.4.2"
                              },
              "node.degga.net" {
                                "pruning" "-",
                                "s" "50002",
                                "version" "1.4.2"
                                },
              "node1.btccuracao.com" {
                                      "pruning" "-",
                                      "s" "50002",
                                      "t" "50001",
                                      "version" "1.4.2"
                                      },
              "nuzzg3pku3xbctgamzq3pf7ztakkiidnmmier64arqwh3ajdddovatad.onion" {
                                                                                "pruning" "-",
                                                                                "s" "50002",
                                                                                "version" "1.4.2"
                                                                                },
              "qly7g5n5t3f3h23xvbp44vs6vpmayurno4basuu5rcvrupli7y2jmgid.onion" {
                                                                                "pruning" "-",
                                                                                "s" "50002",
                                                                                "t" "50001",
                                                                                "version" "1.4.2"
                                                                                },
              "rzspa374ob3hlyjptkdgz6a62wim2mpanuw6m3shlwn2cxg2smy3p7yd.onion" {
                                                                                "pruning" "-",
                                                                                "s" "50004",
                                                                                "t" "50003",
                                                                                "version" "1.4.2"
                                                                                },
              "skbxmit.coinjoined.com" {
                                        "pruning" "-",
                                        "s" "50002",
                                        "t" "50001",
                                        "version" "1.4.2"
                                        },
              "smmalis37.ddns.net" {
                                    "pruning" "-",
                                    "s" "50002",
                                    "version" "1.4.2"
                                    },
              "stavver.dyshek.org" {
                                    "pruning" "-",
                                    "s" "50002",
                                    "t" "50001",
                                    "version" "1.4.2"
                                    },
              "tardis.bauerj.eu" {
                                  "pruning" "-",
                                  "s" "50002",
                                  "t" "50001",
                                  "version" "1.4"
                                  },
              "ty6cgwaf2pbc244gijtmpfvte3wwfp32wgz57eltjkgtsel2q7jufjyd.onion" {
                                                                                "pruning" "-",
                                                                                "s" "50002",
                                                                                "t" "50001",
                                                                                "version" "1.4.2"
                                                                                },
              "udfpzbte2hommnvag5f3qlouqkhvp3xybhlus2yvfeqdwlhjroe4bbyd.onion" {
                                                                                "pruning" "-",
                                                                                "s" "60002",
                                                                                "t" "60001",
                                                                                "version" "1.4.5"
                                                                                },
              "v7gtzf7nua6hdmb2wtqaqioqmesdb4xrlly4zwr7bvayxv2bpg665pqd.onion" {
                                                                                "pruning" "-",
                                                                                "t" "50001",
                                                                                "version" "1.4.2"
                                                                                },
              "v7o2hkemnt677k3jxcbosmjjxw3p5khjyu7jwv7orfy6rwtkizbshwqd.onion" {
                                                                                "pruning" "-",
                                                                                "t" "57001",
                                                                                "version" "1.4.5"
                                                                                },
              "venmrle3xuwkgkd42wg7f735l6cghst3sdfa3w3ryib2rochfhld6lid.onion" {
                                                                                "pruning" "-",
                                                                                "s" "50002",
                                                                                "t" "50001",
                                                                                "version" "1.4.2"
                                                                                },
              "vmd71287.contaboserver.net" {
                                            "pruning" "-",
                                            "s" "50002",
                                            "version" "1.4.2"
                                            },
              "vmd84592.contaboserver.net" {
                                            "pruning" "-",
                                            "s" "50002",
                                            "version" "1.4.2"
                                            },
              "wsw6tua3xl24gsmi264zaep6seppjyrkyucpsmuxnjzyt3f3j6swshad.onion" {
                                                                                "pruning" "-",
                                                                                "s" "50002",
                                                                                "t" "50001",
                                                                                "version" "1.4.2"
                                                                                },
              "xtrum.com" {
                           "pruning" "-",
                           "s" "50002",
                           "t" "50001",
                           "version" "1.4.2"
                           }
              }
             (mapv (fn [[name {s "s" t "t"}]]
                     (when s
                       [name (->> s Integer/parseInt (make-electrum-server name))])
                     ;; (when t
                     ;;   [name (->> t Integer/parseInt (make-electrum-server name))])
                     ))
             (into (ordered-map)))))

(defn grab-electrum-server! []
  (let [[name entry] (first @+electrum-servers+)]
    (swap! +electrum-servers+ dissoc name)
    entry))

;; (defn put-electrum-server! [entry]
;;   (swap! +electrum-servers+ assoc (:name entry) entry))

;;;
;;; Transport - Tor + Socket + SSL
;;;
(defonce +timeouts+ {:slow-read-timeout-secs [] ;; FIXME: implement and use
                     :base-read-timeout-secs []})

(defonce public-server-trust-managers
  (into-array TrustManager [(reify X509TrustManager
                              (getAcceptedIssuers [this]
                                (into-array X509Certificate [])) ;; Return an empty array of accepted issuers
                              (checkClientTrusted [this certs auth-type]) ;; No implementation needed for client certs
                              (checkServerTrusted [this certs auth-type]
                                (when (empty? certs)
                                  (throw (ex-info "No server certificate provided" {})))
                                (.checkValidity (first certs))))]))

(defonce handshake-completed-listener
  (reify HandshakeCompletedListener
    (handshakeCompleted [this event])))

(defprotocol Transport
  (connect [this ssl?])
  (socket->ssl-socket [this]))

(defrecord TorTcpTransport [^InetSocketAddress server ^Proxy proxy timeouts ^Socket socket]
  Transport
  (connect [this ssl?]
    (try
      (let [soc (Socket. proxy)]
        (.connect soc server)
        (.setSoTimeout soc timeouts)
        (cond-> (assoc this :socket soc)
          ssl? (socket->ssl-socket)))
      (catch Exception _
        ;;(remove-electrum-server (.getHostName server))
        )))
  (socket->ssl-socket [{:keys [^Socket socket ^InetSocketAddress server] :as this}]
    (let [ssl-socket (-> (doto (SSLContext/getInstance "TLS")
                           (.init nil public-server-trust-managers (SecureRandom.)))
                         (.getSocketFactory)
                         (.createSocket socket (.getHostName server) (.getPort server) true))]
      (doto ssl-socket
        (.addHandshakeCompletedListener handshake-completed-listener)
        (.startHandshake))

      (assoc this :socket ssl-socket))))

(defonce +active-electrum-sockets+ (atom (ordered-map)))

(defn make-tor-tcp-transport
  ([]
   (let [{:keys [name s] :as entry} (grab-electrum-server!)]
     (make-tor-tcp-transport name s)))
  ([server-addr server-port]
   (make-tor-tcp-transport server-addr server-port "127.0.0.1" 9050))
  ([server-addr server-port proxy-addr proxy-port]
   (try (->  {:server (InetSocketAddress. server-addr server-port)
              :proxy (Proxy. Proxy$Type/SOCKS (InetSocketAddress. proxy-addr proxy-port))
              ;; FIXME: don't need?
              :timeouts 5000}
             map->TorTcpTransport
             (connect true))
        (catch Exception _))))

(defn get-available-socket! []
  (if (empty? @+active-electrum-sockets+)
    (or (make-tor-tcp-transport)
        (recur))
    (let [[name entry] (first @+active-electrum-sockets+)]
      (swap! +active-electrum-sockets+ dissoc name)
      (if (.isClosed (:socket entry))
        (connect entry true)
        entry))))

(defn put-available-socket! [entry]
  (swap! +active-electrum-sockets+ assoc (.getHostName (:server entry)) entry))

(defonce +sub-ch+ (async/chan))
(defonce +sub-sockets+ (atom #{}))
(defonce +subscribe-loop+
  (async/go-loop []
    (doseq [sock @+sub-sockets+]
      (let [socket (:socket sock)]
        (if (.isClosed socket)
          (swap! +sub-sockets+ disj sock)
          (try
            (->> socket .getInputStream InputStreamReader. BufferedReader.
                 .readLine json/read-str (async/>! +sub-ch+))
            (catch Exception _)))))
    (async/<! (async/timeout 1000/4))
    (recur)))

(defn electrum-subscribe [sock method params id]
  (swap! +sub-sockets+ conj sock))

(defn rpc-request [method params]
  (let [sock (get-available-socket!)
        id (gensym "id")]
    (doto (-> sock :socket .getOutputStream OutputStreamWriter. BufferedWriter. PrintWriter.)
      (.println (-> {:method method :params params :id id :jsonrpc "2.0"}
                    json/write-str))
      (.flush))
    (if-let [result (try (-> sock :socket .getInputStream InputStreamReader. BufferedReader. .readLine json/read-str)
                         (catch Exception _))]
      (do
        (put-available-socket! sock)
        (when (str/includes? method ".subscribe")
          (electrum-subscribe sock method params id))
        result)
      (recur method params))))

;; (rpc-request "server.version" [])
;; {:server VPS.hsmiths.com/45.154.252.104:50002, :proxy SOCKS @ /127.0.0.1:9050, :timeouts 5000, :socket SSLSocket[hostname=VPS.hsmiths.com, port=50002, Session(1734329644665|TLS_AES_256_GCM_SHA384)]}
;; echo -e {"method":"sserver.version","params":[],"id":"id19819","jsonrpc":"2.0"} | proxychains nc kareoke.qoppa.org 50001
;; (rpc-request :blockchain.headers.subscribe [])
;; (async/<!! +sub-ch+)
(async/go-loop [[v ch] (async/alts! [+sub-ch+])]
  (println "*** GOT" v))


;; (rpc-request :blockchain.address.get_balance ["3G3rD7bGVqMe2yTztJ8h4FzTk6drznm2bR"])
;; (rpc-request :blockchain.address.get_balance ["1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"])
(rpc-request :server.peers.subscribe [])
;; 1CbFfq7Y2k7cn8L6Y2V4pZ66pRucjuy2aK
;; 1LvqK2czPQsZqk9hCpd57A3t9iJtL8zzyu

(comment
  (defonce +electrum-server-queries+
    {
     :blockchain.address.get_balance [:address] ;; get wallet balance
     :server.version []
     :server.banner []
     :blockchain.headers.subscribe []



     :server.peers.subscribe []
     :blockchain.numblocks.subscribe []

     :blockchain.address.subscribe [:address]
     :blockchain.transaction.get [:tx-hash]
     :blockchain.address.get_history [:address]
     :blockchain.scripthash.get_balance [:scripthash]
     :blockchain.transaction.broadcast [:tx-hash]
     :blockchain.address.listunspent [:address]

     :blockchain.address.get_mempool [??? address]


     blockchain.address.get_proof


     blockchain.utxo.get_address
     blockchain.block.get_header
     blockchain.block.get_chunk


     blockchain.transaction.get_merkle

     blockchain.estimatefee})
  )


;; ### Typical Wallet JSON RPC Operations to Electrum Server

;; When interacting with an Electrum server, a wallet typically needs to perform several operations related to querying balance, getting transaction history, managing addresses, creating transactions, and broadcasting them. Here's a detailed overview of common wallet operations, illustrating the **what**, **why**, **when**, **how**, and the relevant **Electrum JSON-RPC methods**.

;; ---

;; ### 1. **Get Wallet Balance**

;; - **What**: Check the current balance of your wallet.
;; - **Why**: To know how much Bitcoin you own or to decide whether you need to make a transaction (e.g., send Bitcoin or check your available funds).
;; - **When**: Whenever you need to check the balance, especially before sending transactions or reviewing account status.
;; - **How**: Use `blockchain.address.get_balance` to query the balance of a wallet address.

;; **Method**: `blockchain.address.get_balance`

;; **Example Request**:
;; ```json
;; {
;;  "jsonrpc": "2.0",
;;  "method": "blockchain.address.get_balance",
;;  "params": ["1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"]
;;  }
;; ```
;; **Explanation**: Here, you're requesting the balance for a specific Bitcoin address.

;; **Example Response**:
;; ```json
;; {
;;  "jsonrpc": "2.0",
;;  "result": [100000000, 100000000],
;;  "id": 1
;;  }
;; ```
;; - The result contains two values:
;; - **Confirmed balance** (100,000,000 satoshis, which is 1 BTC).
;; - **Unconfirmed balance** (also 1 BTC in this example).

;; ---

;; ### 2. **Get Transaction History**

;; - **What**: Retrieve all the transactions associated with your wallet address.
;; - **Why**: To review past transactions, confirm received payments, or prepare to send coins by viewing the history of previous outputs.
;; - **When**: Use this whenever you need to see past transactions for a wallet or an address.
;; - **How**: Use `blockchain.address.get_history` to get the transaction history for a given address.

;; **Method**: `blockchain.address.get_history`

;; **Example Request**:
;; ```json
;; {
;;  "jsonrpc": "2.0",
;;  "method": "blockchain.address.get_history",
;;  "params": ["1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"]
;;  }
;; ```

;; **Explanation**: You query the Electrum server for the history of the given address.

;; **Example Response**:
;; ```json
;; {
;;  "jsonrpc": "2.0",
;;  "result": [
;;             {"tx_hash": "b63b4328ff9b82adf8ff7f941a7e488e9cc618c828e8a017f900dfab31ee04ea", "height": 640000, "timestamp": 1609459200},
;;             {"tx_hash": "4d2e06c1fbf1c36b648fbf4c9d6c2cf987e7b458d57e97a06cc3e3b87dbd6c75", "height": 641000, "timestamp": 1609545600}
;;             ],
;;  "id": 1
;;  }
;; ```
;; - **Explanation**: This response provides a list of transaction hashes, heights, and timestamps, showing the history of the provided address.

;; ---

;; ### 3. **Create a Transaction (Get Unspent Outputs)**

;; - **What**: Retrieve the unspent transaction outputs (UTXOs) for a specific wallet address. These are the outputs that can be used as inputs for new transactions.
;; - **Why**: To identify available funds for creating a new transaction.
;; - **When**: When creating a new transaction, you need to gather UTXOs associated with your address.
;; - **How**: Use `blockchain.address.get_unspent` to fetch unspent outputs.

;; **Method**: `blockchain.address.get_unspent`

;; **Example Request**:
;; ```json
;; {
;;  "jsonrpc": "2.0",
;;  "method": "blockchain.address.get_unspent",
;;  "params": ["1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"]
;;  }
;; ```

;; **Explanation**: You query the Electrum server for all unspent outputs of the given address.

;; **Example Response**:
;; ```json
;; {
;;  "jsonrpc": "2.0",
;;  "result": [
;;             {"tx_hash": "b63b4328ff9b82adf8ff7f941a7e488e9cc618c828e8a017f900dfab31ee04ea", "tx_pos": 0, "value": 100000000}
;;             ],
;;  "id": 1
;;  }
;; ```
;; - **Explanation**: The response provides the unspent transaction outputs (UTXOs) associated with the address, showing the transaction hash, position in the output, and value (in satoshis).

;; ---

;; ### 4. **Create and Broadcast a Transaction**

;; - **What**: Create a raw Bitcoin transaction and broadcast it to the network.
;; - **Why**: After you've gathered the UTXOs and constructed a transaction, you broadcast it to the Bitcoin network to complete the transfer.
;; - **When**: After building the transaction, you need to broadcast it to the network for miners to include it in a block.
;; - **How**: Use `blockchain.transaction.broadcast` to send the raw transaction to the network.

;; **Method**: `blockchain.transaction.broadcast`

;; **Example Request**:
;; ```json
;; {
;;  "jsonrpc": "2.0",
;;  "method": "blockchain.transaction.broadcast",
;;  "params": ["0200000000010100...f7e74d00"]
;;  }
;; ```

;; **Explanation**: You send the raw transaction (in hexadecimal format) to the server for broadcasting.

;; **Example Response**:
;; ```json
;; {
;;  "jsonrpc": "2.0",
;;  "result": "txid",
;;  "id": 1
;;  }
;; ```
;; - **Explanation**: The response returns the transaction ID (`txid`) of the broadcasted transaction.

;; ---

;; ### 5. **Subscribe to New Headers**

;; - **What**: Subscribe to the latest headers of the Bitcoin blockchain.
;; - **Why**: To receive notifications when new blocks are added to the blockchain, enabling real-time tracking of the blockchain’s progress.
;; - **When**: When you want to track new blocks in real time, for example, to monitor the latest block confirmations.
;; - **How**: Use `blockchain.headers.subscribe` to subscribe to new headers.

;; **Method**: `blockchain.headers.subscribe`

;; **Example Request**:
;; ```json
;; {
;;  "jsonrpc": "2.0",
;;  "method": "blockchain.headers.subscribe",
;;  "params": []
;;  }
;; ```

;; **Explanation**: You're subscribing to receive notifications about new block headers.

;; **Example Response**:
;; ```json
;; {
;;  "jsonrpc": "2.0",
;;  "result": [
;;             "00000000000000000009e60c9c1b3f9cbeed982303f8e5a20b0b84cc2e80eb6b",
;;             1000000,
;;             1558485855
;;             ],
;;  "id": 1
;;  }
;; ```
;; - **Explanation**: The response provides the new block hash, block height, and timestamp of the new block.

;; ---

;; ### 6. **Check Server Status**

;; - **What**: Check if the Electrum server is operational and its sync status.
;; - **Why**: To ensure that the Electrum server is up and running, and it's synced to the Bitcoin network.
;; - **When**: When you want to check if the server is responsive or to confirm if the server is synced with the blockchain.
;; - **How**: Use `server.version` to get the version of the Electrum server and check if it’s responsive.

;; **Method**: `server.version`

;; **Example Request**:
;; ```json
;; {
;;  "jsonrpc": "2.0",
;;  "method": "server.version",
;;  "params": []
;;  }
;; ```

;; **Example Response**:
;; ```json
;; {
;;  "jsonrpc": "2.0",
;;  "result": ["ElectrumX", "1.9.4"],
;;  "id": 1
;;  }
;; ```

;; - **Explanation**: The server responds with the version of the Electrum server, confirming it’s online.

;; ---

;; ### 7. **Get Server Information (e.g., Network Info)**

;; - **What**: Retrieve network or server-specific information, such as the number of connected peers.
;; - **Why**: To ensure that the server has adequate connectivity to the Bitcoin network, which can be helpful for performance and troubleshooting.
;; - **When**: Use this method if you want to monitor server health or assess its connectivity.
;; - **How**: Use `server.peers.subscribe` to get information about connected peers.

;; **Method**: `server.peers.subscribe`

;; **Example Request**:
;; ```json
;; {
;;  "jsonrpc": "2.0",
;;  "method": "server.peers.subscribe",
;;  "params": []
;;  }
;; ```

;; **Example Response**:
;; ```json
;; {
;;  "jsonrpc": "2.0",
;;  "result": [
;;             ["2600:1900:4001:ed::", 50002, 50001, "v1.4.2", "s50002", "t50001"]
;;             ],
;;  "id": 1
;;  }
;; ```

;; - **Explanation**: The server returns information about connected peers, including IP addresses, ports, and protocol versions.

;; ---

;; ### Conclusion:
;; The typical wallet operations in Electrum revolve around querying balances, creating transactions, broadcasting them, and subscribing to blockchain updates. These operations rely on several core JSON-RPC methods such as `blockchain.address.get_balance`, `blockchain.transaction.broadcast`, `blockchain.headers.subscribe

;; `, and `server.version`, among others. Using these methods allows your wallet to interact with the Electrum server for tasks like balance checking, transaction creation, and syncing with the Bitcoin network.
