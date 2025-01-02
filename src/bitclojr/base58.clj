(ns bitclojr.base58
  (:require [buddy.core.hash :as hash]
            [buddy.core.codecs :as codecs]
            [clojure.string :as str]
            [bitclojr.util :as util]))


(defonce +alphabet+ (vec "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"))
(defonce +ch->idx+ (->> +alphabet+ (map-indexed (fn [idx el]
                                                  [el idx]))
                        (into {})))

(defn b58-alpha->idx [alph]
  (get +ch->idx+ alph))

(defonce divmod (juxt quot mod))

(defn- ensure-bytes [hex-or-bytes]
  (cond (bytes? hex-or-bytes) hex-or-bytes
        (string? hex-or-bytes) (codecs/hex->bytes hex-or-bytes)
        :else (throw (ex-info "Hex string or byte array is required"
                              {:input hex-or-bytes}))))

(defn- ->codecs-fn [in]
  (if (= in :bytes)
    identity
    (case in
      :hex codecs/hex->bytes
      :str codecs/str->bytes)))

(defn- <-codecs-fn [out]
  (comp
   (case out
     :bytes identity
     :hex codecs/bytes->hex
     :str codecs/bytes->str)
   byte-array))

(defn encode
  ([in]
   (encode in :hex))
  ([in in-key]
   ;;   "in-key: #{:str :hex :b64 :b64u :byte}
   "in-key: #{:str :hex :b64 :b64u :bytes}"
   (if (empty? in)
     in
     (letfn [(get-base-58-char [i]
               (get +alphabet+ i))]
       (let [b ((->codecs-fn in-key) in)
             big-i (BigInteger. 1 ^bytes b)]
         (loop [[q r] (divmod big-i 58)
                result ()]
           (if (pos? q)
             (recur (divmod q 58) (cons (get-base-58-char r) result))
             (let [result-list (cons (get-base-58-char r) result)
                   count1 (count (take-while #(= \1 %) result-list))
                   count0 (count (take-while #(= 0 %) b))]
               (apply str (into result-list (repeat (- count0 count1) \1)))))))))))

(defn decode [b58-str out-key]
  "out-key: #{:str :hex :b64 :b64u :long :b64-str :bytes}"
  ((<-codecs-fn out-key)
   (cond (empty? b58-str) []
         (= b58-str "1") [0]
         :else
         (loop [i (bigint 1) j (count b58-str) result 0]
           (if (pos? j)
             (recur (* i 58) (dec j) (+ result ^Long (* (b58-alpha->idx (get b58-str (dec j))) i)))
             (let [maybe-signed-bytes (-> result biginteger .toByteArray)
                   result-bytes (if (zero? (first maybe-signed-bytes))
                                  (java.util.Arrays/copyOfRange maybe-signed-bytes 1 (count maybe-signed-bytes))
                                  maybe-signed-bytes)
                   count1 (count (take-while #(= \1 %) b58-str))
                   count0 (count (take-while #(= 0 %) result-bytes))]
               (into (vec (repeat (- count1 count0) 0)) result-bytes)))))))

(defn encode-check
  ([in]
   (encode-check in :hex))
  ([in in-key]
   "in-key: #{:str :hex :b64 :b64u :bytes}"
   (let [bytes ((->codecs-fn in-key) in)]
     (encode (byte-array `[~@bytes ~@(->> bytes util/double-sha256 (take 4))])
             :bytes))))

(defn decode-check [b58-str out-key]
  "out-key: #{:str :hex :b64 :b64u :long :b64-str :bytes}"
  (let [b (decode b58-str :bytes)
        checksum (->> b (take-last 4))
        msg-l (->> (drop-last 4 b))
        actual-checksum (->> msg-l byte-array util/double-sha256 (take 4))]
    (if (= checksum actual-checksum)
      ((<-codecs-fn out-key) msg-l)
      (throw (ex-info "Checksum failed" {:expect checksum
                                         :actual actual-checksum})))))
