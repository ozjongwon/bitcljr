;; https://github.com/BlockchainCommons/Research/blob/master/papers/bcr-2020-005-ur.md
(ns wallet.ur
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [medley.core :as mc]
            [clj-cbor.core :as cbor]
            [buddy.core.codecs :as codecs]))

(defprotocol UrProtocol
  (tag [this])
  (parse [this])
  (->ur-str [this]))

(defonce +bytewords+ "ableacidalsoapexaquaarchatomauntawayaxisbackbaldbarnbeltbetabiasbluebodybragbrewbulbbuzzcalmcashcatschefcityclawcodecolacookcostcruxcurlcuspcyandarkdatadaysdelidicedietdoordowndrawdropdrumdulldutyeacheasyechoedgeepicevenexamexiteyesfactfairfernfigsfilmfishfizzflapflewfluxfoxyfreefrogfuelfundgalagamegeargemsgiftgirlglowgoodgraygrimgurugushgyrohalfhanghardhawkheathelphighhillholyhopehornhutsicedideaidleinchinkyintoirisironitemjadejazzjoinjoltjowljudojugsjumpjunkjurykeepkenokeptkeyskickkilnkingkitekiwiknoblamblavalazyleaflegsliarlimplionlistlogoloudloveluaulucklungmainmanymathmazememomenumeowmildmintmissmonknailnavyneednewsnextnoonnotenumbobeyoboeomitonyxopenovalowlspaidpartpeckplaypluspoempoolposepuffpumapurrquadquizraceramprealredorichroadrockroofrubyruinrunsrustsafesagascarsetssilkskewslotsoapsolosongstubsurfswantacotasktaxitenttiedtimetinytoiltombtoystriptunatwinuglyundouniturgeuservastveryvetovialvibeviewvisavoidvowswallwandwarmwaspwavewaxywebswhatwhenwhizwolfworkyankyawnyellyogayurtzapszerozestzinczonezoom")

(def +first-letter-index+
  (let [word-dist '((a 10) (b 12) (c 14) (d 13) (e 9) (f 15) (g 13) (h 12)
                    (i 9) (j 10) (k 10) (l 15) (m 11) (n 8) (o 7) (p 11) (q 2)
                    (r 12) (s 13) (t 13) (u 5) (v 9) (w 12) (x 0) (y 5) (z 6))]

    (loop [[[s n] & more-w] word-dist wsum 0 result {}]
      (if s
        (recur more-w (+ wsum n) (assoc result (first (str s)) wsum))
        result))))

(defn get-word
  ([n]
   (get-word n :standard))
  ([n encoding]
   (let [i (* n 4)
         w (subs +bytewords+ i (+ i 4))]
     (case encoding
       :standard w
       :minimal (str (first w) (last w))))))

(defn word->index [w]
  (let [start-idx (get +first-letter-index+ (first w))
        comp-fn (case (count w)
                  2 (fn [word]
                      (assert (= (first word) (first w)))
                      (= (last word) (last w)))
                  4 (fn [word]
                      (assert (= (first word) (first w)))
                      (= word w)))]
    (loop [i start-idx]
      (let [word (get-word i)]
        (if (comp-fn word)
          [i word]
          (recur (inc i)))))))

(defn bytewords
  ([hex-str]
   (bytewords hex-str :standard))
  ([hex-str encoding]
   (->> (if (string? hex-str)
          (codecs/hex->bytes hex-str)
          hex-str)
        (map #(get-word (bit-and % 0xff) encoding))
        (apply str))))

(defn encode [ur-inst]
  (let [cbor (->> (parse ur-inst) cbor/encode)
        checksum (-> (doto (java.util.zip.CRC32.)
                       (.update cbor))
                     (.getValue)
                     (codecs/long->bytes)
                     (nthrest 4)
                     (bytewords :minimal))
        bw (bytewords (codecs/bytes->hex cbor) :minimal)]
    (str bw checksum)))

(defn decode [ur-str]
  (let [[fixme d] (str/split ur-str #"/") ;; FIXME
        len (/ (count d) 2)
        d+crc (for [wl (partition 2 d)]
                (let [[i w] (word->index (apply str wl))]
                  i))
        [data checksum] (split-at  (- len  4) d+crc)
        cbor (byte-array data)]
    (assert (= (->> (-> (doto (java.util.zip.CRC32.)
                          (.update cbor))
                        (.getValue)
                        (codecs/long->bytes)
                        (nthrest 4))
                    (map #(bit-and % 0xff)))
               checksum))
    (cbor/decode (update cbor/default-codec :read-handlers merge +ur-tag-handlers+) cbor)))

(comment
  (decode "ur:account-descriptor/oeadcyemrewytyaolttantjyoeadiojojeisdefzdydtaolytantjloxaxhdclaxwmfmdeiamecsdsemgtvsjzcncygrkowtrontzschgezokstswkkscfmklrtauteyaahdcxiehfonurdppfyntapejpproypegrdawkgmaewejlsfdtsrfybdehcaflmtrlbdhpamtantjooeadlncsdwykaeykaeykaocyemrewytyaycynlytsnyltantjyoeadjzjkisdektjojeisdefzdydtdtaolytantjloxaxhdclaostvelfemdyynwydwyaievosrgmambklovabdgypdglldvespsthysadamhpmjeinaahdcxntdllnaaeykoytdacygegwhgjsiyonpywmcmrpwphsvodsrerozsbyaxluzcoxdpamtantjooeadlncsehykaeykaeykaocyemrewytyaycypdbskeuytantjyoeadisktjojeisdefzdydtaolytantjloxaxhdclaxzcfxeegdrpmogrgwkbzctlttweadkiengrwlhtprremouoluutqdpfbncedkynfhaahdcxjpwevdeogthttkmeswzcolcpsaahcfnshkhtehytclmnteatmoteadtlwynnftloamtantjooeadlncsghykaeykaeykaocyemrewytyaycybthlvytstantjyoeadjojkisdeiajljkiniojtihjpdefzdydtdtaolytantjloxaxhdclaxhhsnhdrpftdwuocntilydibehnecmovdfekpjkclcslasbhkpawsaddmcmmnahnyaahdcxlotedtndfymyltclhlmtpfsadscnhtztaolbnnkistaedegwfmmedreetnwmcycnamtantjooeadlfcsdpykaocyemrewytyaycyemrewytytantjyoeadkpjkisdektjkisdeiajljkiniojtihjpdefzdydtdtdtaolytantjloxaxhdclaxdwkswmztpytnswtsecnblfbayajkdldeclqzzolrsnhljedsgminetytbnahatbyaahdcxkkguwsvyimjkvwteytwztyswvendtpmncpasfrrylprnhtkblndrgrmkoyjtbkrpamtantjooeadlocsdyykaeykaeykadykaocyemrewytyaycyhkrpnddrtantjyoeadjsktjkisdeiajljkiniojtihjpdefzdydtdtaolytantjloxaxhdclaohnhffmvsbndslrfgclpfjejyatbdpebacnzokotofxntaoemvskpaowmryfnotfgaahdcxdlnbvecentssfsssgylnhkrstoytecrdlyadrekirfaybglahltalsrfcaeerobwamtantjooeadlocsdyykaeykaeykaoykaocyemrewytyaycyhkrpnddrtantjyoeadiyjyjpdefzdydtaolytantjloxaxhdclaorkrhkeytwsoykorletwstbwycagtbsotmeptjkesgwrfcmveskvdmngujzttgtdpaahdcxgrfgmuvyylmwcxjtttechplslgoegagaptdniatidmhdmebdwfryfsnsdkcplyvaamtantjooeadlncshfykaeykaeykaocyemrewytyaycytostatbnyadleoeh")

  (let [ex "77bff20c60e522dfaa3350c39b030a5d004e839a"]
    (-> (->> ex
             codecs/hex->bytes
             (->Address nil nil)
             ->ur-str
             decode)
        (get 3)
        codecs/bytes->hex
        (= ex)))ur:address/oeadtantjsoeadcsfnaoadaxghlyrlvtmyihryykielnamspnlmkptsflyieeskoflkovdfdlb


  (->ur-str (->Psbt (codecs/hex->bytes "70736274ff01009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f000000000000000000")))
  )

(defonce +ur-tag-handlers+ {})

(defmacro def-ur [name [& fields] & body]
  `(do (defrecord ~name [~@fields]
         UrProtocol
         ~@body)
       (alter-var-root #'+ur-tag-handlers+
                       (fn [x#]
                         (let [inst# ((resolve (symbol (str "map->" '~name))) {})]
                           (assoc x#
                                  (tag inst#)
                                  identity))))))

(def-ur Address [info type data]
  (tag [this]
       40307)
  (parse [this]
         (mc/assoc-some {3 (if (string? data)
                             (codecs/hex->bytes data)
                             data)} 1 info 2 type))
  (->ur-str [this]
            (str "ur:address/" (encode this))))

(def-ur Psbt [bytes]
  (tag [this]
       40310)
  (parse [this]
         bytes)
  (->ur-str [this]
            (str "ur:psbt/" (encode this)))  )

(def-ur Seed [payload creation-date name note]
  (tag [this]
       40300)
  (parse [this]
         (mc/assoc-some {1 payload} 2 creation-date 3 name 4 note))
  (->ur-str [this]
            (str "ur:seed/" (encode this)))  )

(def-ur HdKey [master-fingerprint output-descriptors]
  UrProtocol
  (tag [this]
       40303)
  (parse [this]
         bytes)
  (->ur-str [this]
            (str "ur:account-descriptor/" (encode this)))  )

(def-ur KeyPath [master-fingerprint output-descriptors]
  UrProtocol
  (tag [this]
       40304)
  (parse [this]
         bytes)
  (->ur-str [this]
            (str "ur:account-descriptor/" (encode this)))  )

(def-ur OutputDescriptor [master-fingerprint output-descriptors]
  UrProtocol
  (tag [this]
       40308)
  (parse [this]
         bytes)
  (->ur-str [this]
            (str "ur:account-descriptor/" (encode this)))  )

(def-ur Account [master-fingerprint output-descriptors]
  UrProtocol
  (tag [this]
       40311)
  (parse [this]
         bytes)
  (->ur-str [this]
            (str "ur:account-descriptor/" (encode this)))  )
