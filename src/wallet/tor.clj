;; https://github.com/spesmilo/electrum/blob/master/electrum/servers.json

(ns wallet.tor
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            ;;[clj-okhttp.core :as http]
            )
  (:import [java.net InetSocketAddress Proxy Proxy$Type Socket]
           [javax.net.ssl SSLContext SSLSocketFactory SSLSocket TrustManager X509TrustManager
            HandshakeCompletedListener SSLSocket]
           [java.security.cert X509Certificate]
           [java.io BufferedReader InputStreamReader OutputStreamWriter]
           [java.security SecureRandom]))

(defonce electrum-onions
  {"wsw6tua3xl24gsmi264zaep6seppjyrkyucpsmuxnjzyt3f3j6swshad.onion"
   {"pruning" "-", "s" "50002", "t" "50001", "version" "1.4.2"},
   "22mgr2fndslabzvx4sj7ialugn2jv3cfqjb3dnj67a6vnrkp7g4l37ad.onion"
   {"pruning" "-", "t" "50001", "version" "1.4.2"},
   "egyh5mutxwcvwhlvjubf6wytwoq5xxvfb2522ocx77puc6ihmffrh6id.onion"
   {"pruning" "-", "s" "50002", "t" "50001", "version" "1.4.2"},
   "udfpzbte2hommnvag5f3qlouqkhvp3xybhlus2yvfeqdwlhjroe4bbyd.onion"
   {"pruning" "-", "s" "60002", "t" "60001", "version" "1.4.5"},
   "ty6cgwaf2pbc244gijtmpfvte3wwfp32wgz57eltjkgtsel2q7jufjyd.onion"
   {"pruning" "-", "s" "50002", "t" "50001", "version" "1.4.2"},
   "rzspa374ob3hlyjptkdgz6a62wim2mpanuw6m3shlwn2cxg2smy3p7yd.onion"
   {"pruning" "-", "s" "50004", "t" "50003", "version" "1.4.2"},
   "kittycp2gatrqhlwpmbczk5rblw62enrpo2rzwtkfrrr27hq435d4vid.onion"
   {"pruning" "-", "s" "50002", "t" "50001", "version" "1.4.2"},
   "venmrle3xuwkgkd42wg7f735l6cghst3sdfa3w3ryib2rochfhld6lid.onion"
   {"pruning" "-", "s" "50002", "t" "50001", "version" "1.4.2"},
   "v7gtzf7nua6hdmb2wtqaqioqmesdb4xrlly4zwr7bvayxv2bpg665pqd.onion"
   {"pruning" "-", "t" "50001", "version" "1.4.2"},
   "nuzzg3pku3xbctgamzq3pf7ztakkiidnmmier64arqwh3ajdddovatad.onion"
   {"pruning" "-", "s" "50002", "version" "1.4.2"},
   "bejqtnc64qttdempkczylydg7l3ordwugbdar7yqbndck53ukx7wnwad.onion"
   {"pruning" "-", "s" "50002", "t" "50001", "version" "1.4.5"},
   "v7o2hkemnt677k3jxcbosmjjxw3p5khjyu7jwv7orfy6rwtkizbshwqd.onion"
   {"pruning" "-", "t" "57001", "version" "1.4.5"},
   "qly7g5n5t3f3h23xvbp44vs6vpmayurno4basuu5rcvrupli7y2jmgid.onion"
   {"pruning" "-", "s" "50002", "t" "50001", "version" "1.4.2"},
   "explorerzydxu5ecjrkwceayqybizmpjjznk5izmitf2modhcusuqlid.onion"
   {"pruning" "-", "t" "110", "version" "1.4"}})

(defonce +timeouts+ {:slow-read-timeout-secs []
                     :base-read-timeout-secs []})

(def public-server-trust-managers
  (into-array TrustManager [(reify X509TrustManager
                              (getAcceptedIssuers [this]
                                (println "***1")
                                (into-array X509Certificate [])) ;; Return an empty array of accepted issuers
                              (checkClientTrusted [this certs auth-type]
                                (println "***2")) ;; No implementation needed for client certs
                              (checkServerTrusted [this certs auth-type]
                                (println "***3")
                                (when (empty? certs)
                                  (throw (ex-info "No server certificate provided" {})))
                                (.checkValidity (first certs))))]))

(def handshake-completed-listener
  (reify HandshakeCompletedListener
    (handshakeCompleted [this event]
      (println "***4"))))

(defprotocol Transport
  (connect [this ssl?])
  (socket->ssl-socket [this]))

(defrecord TorTcpTransport [^InetSocketAddress server ^Proxy proxy timeouts ^Socket socket]
  Transport
  (connect [this ssl?]
    (let [soc (Socket. proxy)]
      (.connect soc server)
      (.setSoTimeout soc timeouts)
      (cond-> (assoc this :socket soc)
        ssl? (socket->ssl-socket))))
  (socket->ssl-socket [{:keys [^Socket socket ^InetSocketAddress server] :as this}]
    (let [ssl-socket (-> (doto (SSLContext/getInstance "TLS")
                           (.init nil public-server-trust-managers (SecureRandom.)))
                         (.getSocketFactory)
                         (.createSocket socket (.getHostName server) (.getPort server) true))]
      (doto ssl-socket
        (.addHandshakeCompletedListener handshake-completed-listener)
        (.startHandshake))

      (assoc this :socket ssl-socket))))

(defn make-tor-tcp-transport
  ([server-addr server-port]
   (make-tor-tcp-transport server-addr server-port "127.0.0.1" 9050))
  ([server-addr server-port proxy-addr proxy-port]
   (map->TorTcpTransport {:server (InetSocketAddress. server-addr server-port)
                          :proxy (Proxy. Proxy$Type/SOCKS (InetSocketAddress. proxy-addr proxy-port))
                          :timeouts 5000})))

(def onion1 (make-tor-tcp-transport "wsw6tua3xl24gsmi264zaep6seppjyrkyucpsmuxnjzyt3f3j6swshad.onion" 5002
                                    "127.0.0.1" 9050))

;;;;;;;;;;;;;;;;;;;;;;;
(def electrum-hosts
  (atom ["signet-electrumx.wakiyamap.dev"
         "testnet.aranguren.org"
         "testnet.hsmiths.com"
         "testnet.qtornado.com" "tn.not.fyi"]))

(defn pick-electrum-server []
(let [host (first @electrum-hosts)]
  ;; rotate
  (swap! electrum-hosts #(conj (subvec % 1) host))
  host))

(defn rpc-request [msg]
(let [[host port] [(pick-electrum-server) 50001]
      tor-proxy (Proxy. Proxy$Type/SOCKS (InetSocketAddress. "127.0.0.1" 9050)) ; Tor SOCKS proxy
      socket (Socket. tor-proxy) ; Connect through Tor
      ]
  (try (let [in-stream (do (.connect socket (InetSocketAddress. host port))
                           (BufferedReader. (InputStreamReader. (.getInputStream socket))))
             out-stream (OutputStreamWriter. (.getOutputStream socket))]
         (println "Connected to" host "through Tor on port" port)

         ;; Send data to the server
         (.write out-stream msg)
         (.flush out-stream)

         ;; Receive response from the server
         (println "Server response: " (.readLine in-stream))

         ;; Close the socket
         (.close socket))
       (catch Exception _
         (println "Delete" host ":" port)))))

(rpc-request "{\"method\":\"server.version\",\"params\":[],\"id\":\"id23042\",\"jsonrpc\":\"2.0\"}\r\n")

;; (defn create-tcp-client-through-tor [host port]
;;   (let [tor-proxy (Proxy. Proxy$Type/SOCKS (InetSocketAddress. "127.0.0.1" 9050)) ; Tor SOCKS proxy
;;         socket (Socket. tor-proxy) ; Connect through Tor
;;         _ (.connect socket (InetSocketAddress. host port))
;;         in-stream (BufferedReader. (InputStreamReader. (.getInputStream socket)))
;;         out-stream (OutputStreamWriter. (.getOutputStream socket))]

;;     (println "Connected to" host "through Tor on port" port)

;;     ;; Send data to the server
;;     (.write out-stream "{\"method\":\"server.version\",\"params\":[],\"id\":\"id23042\",\"jsonrpc\":\"2.0\"}\r\n")
;;     (.flush out-stream)

;;     ;; Receive response from the server
;;     (println "Server response: " (.readLine in-stream))

;;     ;; Close the socket
;;     (.close socket)))






















(comment
  (def tor-client (http/create-client {:proxy (Proxy. Proxy$Type/SOCKS
                                                      (InetSocketAddress. "127.0.0.1" 9050))}))

  ;; (http/post client "http://222dcrzbvqyl3btwkmhny5nyysmpnindp2ne5zjgorvtute4igictjqd.onion:8332"
  ;;            {:as :json
  ;;             :body {:jsonrpc "1.0" :id "id101" :method "getblockchaininfo" :params []}
  ;;             :headers {"content-type" "application/json"}})

  (defonce electrum-hosts
    (atom ["signet-electrumx.wakiyamap.dev" "blackie.c3-soft.com"
           ;; "blockstream.info"
           "electrum.blockstream.info"
           "testnet.aranguren.org"
           ;; "explorerzydxu5ecjrkwceayqybizmpjjznk5izmitf2modhcusuqlid.onion"
           ;; "testnet.hsmiths.com"
           ;; "testnet.qtornado.com" "tn.not.fyi"
           ]))

  (defn pick-electrum-server []
    (let [host (first @electrum-hosts)]
      ;; rotate
      (swap! electrum-hosts #(conj (subvec % 1) host))
      host))

  (defonce protocol (gloss/compile-frame (gloss/delimited-frame ["\n" "\r\n" "\r"] (gloss/string :utf-8))))

  (def client (http/create-client {}))
  (defn query-electrum-server
    ([method params]
     (let [id (str (gensym "id"))]
       (-> {:method method :params params :id id :jsonrpc "2.0"}
           json/write-str
           (str "\r\n")
           (query-electrum-server (pick-electrum-server) id))))
    ([msg electr id]
     (http/post client (str "https://" electr ":50001")
                {:as :json
                 :body msg
                 :headers {"content-type" "application/json"}})   ))

  ;;
  (comment



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
  ;; https://raw.githubusercontent.com/bitcoin/bitcoin/9039d8f1a1dfe080321f119e1bda2255e1cfdeb9/contrib/seeds/nodes_main.txt

  (import [org.apache.commons.codec.binary Base32])
  (require '[buddy.core.codecs :as codecs])

  (defn decode-base32 [base32-str]
    "Decode a Base32-encoded string into a byte array."
    (let [base32-decoder (Base32.)]
      (-> (.decode base32-decoder base32-str)
                                        ;(String. byte-array "UTF-8")
          )))

  (decode-base32 "23zqyu5dxq3gilx5dihlyjh6tbkwombyaks4pbeyg2pqjvv7putq")




  (http/get cli "http://juhanurmihxlp77nkq76byazcldy2hlmovfu2epvl5ankdibsot4csyd.onion/")



  (http/post cli "http://3dioxfoojb3gmink7s4mqjggdv3nzvukzzojg4elpigwfv4kos54m6qd.onion:8333"
             {:body (json/write-str {:jsonrpc "1.0" :id "id101" :method "getblockchaininfo" :params []})
              :headers {"content-type" "application/json"
                        "accept" "application/json"}})



  (def onions ["http://3dlf22xklplcrmwu7mwajukqemfu7jew75zysguei3g5cdmhppb2idid.onion:8333"
               "http://3dnisrzkr33yhs76q2h4geoks4a5v3g4uc7gnf7typpk47ndnbv5i6qd.onion:8333"
               "http://3dqk7tzfjtasfdouwizp3zouax6vc7bojqvecin3k64sxnhdsdcoijad.onion:8333"
               "http://3dvsn467m3rpyulqpso7yflpjvazzdfwfz6xmoyr6gokyblh6xqzeuid.onion:8333"
               "http://3dwshqqmo6zejyp6xqm2y2ehke34xvkvt2dxwkokuve523rx5sqmgiad.onion:8333"
               "http://3dxq5z3rmhzhr5jaaiwzn34dlxjashwdfyezp3pxi2bokf6ljlrysuid.onion:8333"
               "http://3dzdeacij3q4trp6l6o2dsfjx4cz4tloxh5vphhwdhpaezyqd4hexnad.onion:8333"
               "http://3e26svvwgzcjdrxfhcuqwxkvmbphl637mj3fnzaxa6ixdwpwh5p5k3qd.onion:8333"
               "http://3e5rca4xswwqv73at55usgyrd4z5jhchi6y3ebpuwbrn6mxksx6lajyd.onion:8333"
               "http://3e6kw36mvrkjvn6bnpfw7p5zk3up4siirtln53wcdz6zimm7wysxlyyd.onion:8333"
               "http://3eadceuwihx4txoucokxnevgz4dykoyfutc6f26hnyjab4ruspv2stad.onion:8333"
               "http://3eebrkl75sc2aixnck3frscwlueukqim74yfhrhxtpgqpjnfsttocuad.onion:8333"
               "http://3efkcoestpjrp5dzy3r2jzie4azrgchmuq6sq5twzzoaj7ntexjugnad.onion:8333"
               "http://3ehn4dkcebwppuf5guzhfctd3kjnaen6vnlefmvz5vwmwwgp5ko6chid.onion:8333"
               "http://3ehzljbd2mkek57rtniqlyn27by3r6jl462pswuj2yufsgnluzvg6xyd.onion:8333"
               "http://3ejjlrtljngyujusbhzk5rq634jjf3t7ew6wwbbpoiz6jvrvesgm3hyd.onion:8333"
               "http://3ejuweecask5bzofgi2icszj6sakrnykoowhuugz7aaxysensbdoteqd.onion:8333"
               "http://3enlacw5vbch3bguintmnohw6fzu4hoeqi7a2ioxpdppl5by3w5xriqd.onion:8333"
               "http://3er6ensmrsnvetktxlv6eh7zwudq6u52qelgl7w5waobj4yrkzh42ayd.onion:8333"
               "http://3f44ddpy2c6wytq37d73ro7j3hl24njgmkhknxyekd3nfi6fi62adhad.onion:8333"
               "http://3f73jsy4xqdurrqgqnrecl6bxsbk35qsn666oymh4vxx3ieog3ekurqd.onion:8333"
               "http://3famfg4cpbtyql74darakfhu7pb7354iqwrgjhojahbi2kpmsh45y3id.onion:8333"
               "http://3fcoiesortylumk22xm3xghyrvbvwdmipjhm56exktncgmoapr6dciqd.onion:8333"
               "http://3feeiighwx357lpfketp4ojlghz6z3wwbimikx72ozj2ikd4cb3i35id.onion:8333"
               "http://3fk6g56e3fxxttlsi2kvqtjyqrtgbe72xnbdabikex7xb2kzfmti4myd.onion:8333"
               "http://3fkgokadhndc5wvipluurymfskvjgpqmznu2jrxunymqohrqmfyjibyd.onion:8333"
               "http://3frloojz7ikwijeg4orohhf5lpcze4ujauoqpau3xrdrmngnfz4l6lyd.onion:8333"
               "http://3frwvyzgvk3jnojqfnxr7gv3nmx6mucresfwagx7y2z4457eiytqopad.onion:8333"
               "http://3ftudomzezba7k4q6u42lsb2g735d3edcsssx3p737ckbo53dn5ezgyd.onion:8333"
               "http://3fvo3tm7uoyox3jishbkvw3gipe54rokxmszcqgl5g6xd6qfuunelwqd.onion:8333"
               "http://3fwl5v6iahfwddwsihkwuyhusnyohslzhv3vn3wcupsiufnpjv6zhgad.onion:8333"
               "http://3fz7duies3jdutkjdznyoi3ycirydwj5xlp3taz42bwkhikeyg43tgyd.onion:8333"
               "http://3g2rrzpy2xl5ny2kxysa576a3lbb2v6joqxv72v3ibjjxws5tc3lhtyd.onion:8333"
               "http://3g2xicb2jik75coe7fif77pnhiptfcuzu2nujq2trk6xqfrscpiox6ad.onion:8333"
               "http://3g746n2vwstnw2hvjzlljge3lto62sgmxshdbfaj2e7jjdhjtq4kv2qd.onion:8333"
               "http://3g776hibrzdjsiw4spypnhxdyyarvlz7bhgbqf5pyeuitalx66s5r2ad.onion:8333"
               "http://3gf2vurla7entuwpbqes3lsi7gz4b244rki7yzsagvqiyajtq4f3glqd.onion:8333"
               "http://3ggundljom2tvsjr47ruarb6mlxssafayhdnmqhv6wmsuhcj567rudad.onion:8333"
               "http://3gimt6cplisuuilobs5x5xf5nwjqdzkdzzdkbigmoi7352yzhxww6lyd.onion:8333"
               "http://3gu5fqif5ymac7pejh7k5nm44kinli4dte4z3vdo4qh5xyrnbladmgyd.onion:8333"
               "http://3gwnapuz7wc6xcnoz7dvq252bka2bp57bkfebcyh4gjtksdl6z62r4ad.onion:8333"
               "http://3gyuc24ccsxdofucyonsncrcy7ptxwpaf5vfghskvf7csyptqeczt6qd.onion:8333"
               "http://3h2peqno626evpniklqemlv2p6vzfommkovsag75p5w27y5ej4dfowqd.onion:8333"
               "http://3hacynqewkcqq7rhwtqq27dcdxljnhxa7xfv4blx5bjyhvothmvoc7yd.onion:8333"
               "http://3hajb4fpe7ylbwcdtq2z2aunikro6eg3xvsp34bbj7wadhrricbwy3id.onion:8333"
               "http://3hc4pyokzisar3w5xpjiboh5f6zou4youew4qilvhpxadcqpabiwdhqd.onion:8333"
               "http://3heremuxxvifb6nzwe2vdjjrbbpmg7b2nctlt45bd3ezpumuhjvl2hid.onion:8333"
               "http://3hhfj4hvqh2q4mqlnveptxagrtlmmtiuvowuyqfw2olvrm5i6ptzuzid.onion:8333"
               "http://3hkobt2nflzjhwqtdmileemqh5ls6poovnegai4ut67z7i4e2tbloxad.onion:8333"])

  (for [addr onions
        :let [result (try (http/post cli "http://explorerzydxu5ecjrkwceayqybizmpjjznk5izmitf2modhcusuqlid.onion"
                                     {:as :json
                                      :body (json/write-str {:jsonrpc "1.0" :id "id101" :method "getblockchaininfo" :params []})
                                      :headers {"content-type" "application/json"
                                                "accept" "application/json"}})
                          (catch Exception _
                            nil))]
        :when result]
    [addr result])
  )
