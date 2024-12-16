;; https://github.com/spesmilo/electrum/blob/master/electrum/servers.json

(ns wallet.tor
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            ;;[clj-okhttp.core :as http]
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
      entry)))

(defn put-available-socket! [entry]
  (swap! +active-electrum-sockets+ assoc (.getHostName (:server entry)) entry))

(defn rpc-request [method params]
  (let [sock (get-available-socket!)]
    (doto (-> sock :socket .getOutputStream OutputStreamWriter. BufferedWriter. PrintWriter.)
      (.println (-> {:method method :params params :id (str (gensym "id")) :jsonrpc "2.0"}
                    json/write-str))
      (.flush))
    (let [in-stream (-> sock :socket .getInputStream InputStreamReader. BufferedReader.)
          result (-> in-stream .readLine json/read-str)]
      (put-available-socket! sock)
      result)))

;; (rpc-request "server.version" [])
;; {:server VPS.hsmiths.com/45.154.252.104:50002, :proxy SOCKS @ /127.0.0.1:9050, :timeouts 5000, :socket SSLSocket[hostname=VPS.hsmiths.com, port=50002, Session(1734329644665|TLS_AES_256_GCM_SHA384)]}
;; echo -e {"method":"sserver.version","params":[],"id":"id19819","jsonrpc":"2.0"} | proxychains nc kareoke.qoppa.org 50001
