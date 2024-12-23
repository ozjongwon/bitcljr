(ns wallet.base58
  (:require [buddy.core.hash :as hash]
            [buddy.core.codecs :as codecs]
            [clojure.string :as str]
            [wallet.util :as util]))

(defn double-sha256 [bytes]
  (-> bytes
      hash/sha256
      hash/sha256))

(defonce +alphabet+ (vec "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"))
(defonce +ch->idx+ (->> +alphabet+ (map-indexed (fn [idx el]
                                                  [el idx]))
                        (into {})))

(defn b58-alpha->idx [alph]
  (get +ch->idx+ alph))

(defonce divmod (juxt quot mod))

(defn encode [hex-str]
  (letfn [(get-base-58-char [i]
            (get +alphabet+ i))]
    (loop [[q r] (-> hex-str
                     codecs/hex->bytes
                     BigInteger.
                     (divmod 58))
           result ()]
      (if (pos? q)
        (recur (divmod q 58) (cons (get-base-58-char r) result))
        (let [result-list (cons (get-base-58-char r) result)
              count0 (count (take-while #(= \0 %) hex-str))]
          (apply str (into result-list (repeat (quot count0 2) \1))))))))


(defn encode [hex-or-bytes]
  (letfn [(get-base-58-char [i]
            (get +alphabet+ i))]
    (loop [[q r] (-> (if (string? hex-or-bytes)
                       (codecs/hex->bytes hex-or-bytes)
                       hex-or-bytes)
                     BigInteger.
                     (divmod 58))
           result ()]
      (if (pos? q)
        (recur (divmod q 58) (cons (get-base-58-char r) result))
        (let [result-list (cons (get-base-58-char r) result)
              count0 (count (take-while #(= \0 %) hex-or-bytes))]
          (apply str (into result-list (repeat (quot count0 2) \1))))))))

(defn decode [b58-str]
  (loop [i (biginteger 1) j (dec (count b58-str)) result 0]
    (if (< j 0)
      (-> result
          biginteger
          .toByteArray
          codecs/bytes->hex)
      (recur (* i 58) (dec j) (+ result (* (b58-alpha->idx (get b58-str j)) i))))))

(defn encode-check [hex-str]
  (let [bytes (codecs/hex->bytes hex-str)]
    (encode (byte-array `[~@bytes ~@(->> bytes double-sha256 (take 4))]))))

(defn decode-check [s]
  (let [b (->> (decode s) (codecs/hex->bytes))
        checksum (->> b (take-last 4))
        msg-l (->> (drop-last 4 b))
        actual-checksum (->> msg-l byte-array double-sha256 (take 4))]
    (if (= checksum actual-checksum)
      (codecs/bytes->hex (byte-array msg-l))
      (throw (ex-info "Checksum failed" {:expect checksum
                                         :actual actual-checksum})))))
