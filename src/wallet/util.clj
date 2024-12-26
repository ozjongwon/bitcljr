(ns wallet.util
  (:require [buddy.core.hash :as hash]))

(defn hash160 [x]
  (-> x hash/sha256 hash/ripemd160))

(defn signed->unsigned [v]
  (bit-and v 0xff))

(defn unsigned->signed [val]
  (if (> val 127)
    (- val 256)
    val))

(defn ->n-byte-array [i n]
  (if (bytes? i)
    i
    (let [bytes (byte-array n)]
      (loop [idx (dec n) v i]
        (if (< idx 0)
          bytes
          (do (->> 0xff
                   (bit-and v)
                   unsigned->signed
                   (aset-byte bytes idx))
              (recur (dec idx) (bit-shift-right v 8))))))))

(defn bytes->int [barr]
  (reduce
   (fn [acc byte]
     (bit-or (bit-shift-left acc 8) (bit-and byte 0xFF)))  ; Shift left and combine bytes
   0
   barr))

(defn position
  ([v el]
   (position v el 0))
  ([v el start]
   (loop [[x & xs] (nthrest v start) idx start]
     (cond (= x el) idx
           x (recur xs (inc idx))
           :else nil))))
