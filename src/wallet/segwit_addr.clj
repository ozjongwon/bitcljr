(ns wallet.segwit-addr
  (:require [clojure.string :as str]
            [wallet.bech32 :as bc32]))

(defprotocol MergeSplit
  (bit-merge [this bits])
  (bit-split [this n]))

(defrecord BitData [data size]
  MergeSplit
  (bit-merge [this {data-b :data size-b :size}]
    (assoc this
           :data (bit-or (bit-shift-left data size-b) data-b)
           :size (+ size size-b)))
  (bit-split [this n]
    [(assoc this
            :data (bit-shift-right data (- size n))
            :size n)
     (let [size-b  (- size n)]
       (assoc this
              :data (bit-and data (dec (bit-shift-left 1 size-b)))
              :size size-b))]))

(defn make-bit-data [n n-bits]
  (->BitData n n-bits))

(comment
  (let [bit1 (make-bit-data 2r11010101 8)
        [bit2 bit3] (bit-split bit1 5)]
    [(Integer/toString (:data bit2) 2)
     (Integer/toString (:data bit3) 2)]))

(defn n-bits->m-bits [data from-bits to-bits]
  (letfn [(process-carry [carry]
            (loop [{:keys [data size] :as bit-data} carry result []]
              (cond (zero? size) [result nil]
                    (= size to-bits) [(conj result data) nil]
                    (< size to-bits) [result (make-bit-data data size)]
                    :else (let [[a b] (bit-split bit-data to-bits)]
                            (recur b (conj result (:data a)))))))]
    (loop [[value & more-values] data
           carry (make-bit-data 0 0)
           result []]
      (if value
        (let [[data-to-add next-carry] (->> from-bits
                                            (make-bit-data value)
                                            (bit-merge carry)
                                            process-carry)]
          (recur more-values next-carry (into result data-to-add)))
        (let [[carry-data {:keys [data size]}] (process-carry carry)]
          (if (zero? data)
            (into result carry-data)
            (->> size
                 (- to-bits)
                 (bit-shift-left data) ;; padding
                 (conj carry-data)
                 (into result))))))))

(comment
  (convert-bits [2r11010101 2r01010101] 8 5)
  (convert-bits [2r11110000 2r10101010] 8 5)
  (convert-bits [2r10011001 2r11100011] 8 5)
  (convert-bits [2r00010101 2r11001100] 8 5)
  (convert-bits [2r01101001 2r10011010] 8 5))
