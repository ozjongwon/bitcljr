(ns wallet.base58)

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
  (pipeline 256 58 byte alphabet zero? (first alphabet) value))

(defn decode [value]
  (->> (drop-while first-char? value)
       (pipeline 58 256 inverted-alphabet char first-char? "\000")))
