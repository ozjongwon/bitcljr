;;;
;;; Borrowed from https://github.com/rm-hull/base58
;;;

(ns wallet.base58
  (:require [buddy.core.hash :as hash]))

(defn double-sha256 [bytes]
  (-> bytes
      hash/sha256
      hash/sha256))

(defonce alphabet (vec "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"))

(def inverted-alphabet
  (into {}
        (map #(vector %1 %2)
             alphabet
             (iterate inc 0))))

(defn count-leading [pred s]
  (->> s
       (map byte)
       (take-while pred)
       count))

(defn string->bigint [base xform s]
  (reduce +
          (map #(* (xform %1) %2)
               (reverse s)
               (iterate (partial * base) 1M))))

(def divmod (juxt quot mod))

(def first-char? (partial = (byte (first alphabet))))

(defn emitter [base value]
  (if (pos? value)
    (let [[d m] (divmod value base)]
      (cons
       (int m)
       (lazy-seq (emitter base d))))))

(defn pipeline [from to xform map-fn drop-pred replace-ch s]
  (->> s
       (string->bigint from xform)
       (emitter to)
       (map map-fn)
       reverse
       (concat (repeat (count-leading drop-pred s) replace-ch))
       (apply str)))

(defn encode [value]
  (pipeline 256 58 #(bit-and % 0xff) alphabet zero? (first alphabet) value))

(defn decode [value]
  (->> (drop-while first-char? value)
       (pipeline 58 256 inverted-alphabet char first-char? "\000")))

(defn encode-check [v]
  (encode `[~@v ~@(->> v (double-sha256) (take 4) ;(map #(bit-and % 0xff))
                       )]))

(defn decode-check [s]
  (let [b (decode s)
        checksum (take-last 4 b)
        msg (drop-last 4 b)]
    (if (= checksum
           (->> msg byte-array double-sha256 (take 4)))
      (byte-array msg)
      (throw (ex-info "Checksum failed" {:expect checksum})))))
