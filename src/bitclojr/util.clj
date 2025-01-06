(ns bitclojr.util
  (:require [buddy.core.hash :as hash]
            [buddy.core.codecs :as codecs]))


(defn ensure-bytes [in]
  (cond (bytes? in) in
        (sequential? in) (byte-array in)
        (string? in) (codecs/hex->bytes in)
        :else (throw (ex-info "Hex string or byte array is required"
                              {:input in}))))

(defn ensure-vector [seq-or-bytes]
  (if (vector? seq-or-bytes)
    seq-or-bytes
    (vec seq-or-bytes)))

(defn hash160 [x]
  (-> x hash/sha256 hash/ripemd160))

(defn double-sha256 [bytes]
  (-> bytes
      hash/sha256
      hash/sha256))

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

(defn ->n-vector [i n]
  (loop [idx (dec n) v i result []]
    (if (< idx 0)
      result
      (recur (dec idx) (bit-shift-right v 8) (->> 0xff
                                                  (bit-and v)
                                                  unsigned->signed
                                                  (conj result))))))

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

(defn sort-arrays [k arrays]
  (letfn [(array-comp [a1 a2]
            (let [alen (count a1)]
              (assert (= alen (count a2)))
              (loop [i 0]
                (if (= i alen)
                  0
                  (let [diff (compare (bit-and (aget a1 i) 0xff)
                                      (bit-and (aget a2 i) 0xff))]
                    (if (zero? diff)
                      (recur (inc i))
                      diff))))))]
    (sort-by k array-comp arrays)))

(comment
  (sort-arrays identity [(buddy.core.codecs/hex->bytes "022df8750480ad5b26950b25c7ba79d3e37d75f640f8e5d9bcd5b150a0f85014da")
                         (buddy.core.codecs/hex->bytes "03e3818b65bcc73a7d64064106a859cc1a5a728c4345ff0b641209fba0d90de6e9")
                         (buddy.core.codecs/hex->bytes "021f2f6e1e50cb6a953935c3601284925decd3fd21bc445712576873fb8c6ebc18")])
  )
