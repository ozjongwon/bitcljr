(ns wallet.segwit-addr
  (:require [clojure.string :as str]
            [wallet.bech32 :as bc32]))

#_
(defn convert-bits [data from-bits to-bits padding?]
  (loop [[value & more-values] data n-carry 0 carry 0 result []]
    (if value
      (let [total-bits (+ n-carry from-bits)]
        (when (or (neg? value) (pos? (bit-shift-right value from-bits)))
          (throw (ex-info "Invalid data - does not fit into from-bits" {:data value})))
        (cond (<= to-bits n-carry) ;; don't need to consume 'value'
              (let [next-n-bits (- n-carry to-bits)]
                (recur (cons value more-values)
                       next-n-bits
                       (->> next-n-bits
                            (bit-shift-left 1)
                            dec
                            (bit-and carry))
                       (conj result (bit-shift-right carry (- n-carry to-bits)))))

              (>= total-bits to-bits) ;; consume & merge with carry
              (let [merged (bit-or (bit-shift-left carry from-bits) value)
                    next-n-bits (- total-bits to-bits)]
                (recur more-values
                       next-n-bits
                       (->> next-n-bits
                            (bit-shift-left 1)
                            dec
                            (bit-and merged))
                       (conj result (bit-shift-right merged next-n-bits))))

              :else  ;; total-bits < to-bits
              (recur more-values
                     total-bits
                     (bit-or (bit-shift-left carry from-bits) value)
                     result)))
      (if (zero? carry)
        result
        (conj result carry)))))

#_
(defn convert-bits [data from-bits to-bits padding?]
  (loop [[value & more-values] data n-carry 0 carry 0 result []]
    (println "***" value n-carry carry result)
    (if (or value (pos? n-carry))
      (let [value (or value 0)]
        (when (or (neg? value) (pos? (bit-shift-right value from-bits)))
          (throw (ex-info "Invalid data - does not fit into from-bits" {:data value})))

        (if (<= to-bits n-carry) ;; don't need to consume 'value'
          (let [next-n-bits (- n-carry to-bits)]
            (println "***111" n-carry carry)
            (recur (cons value more-values)
                   next-n-bits
                   (->> next-n-bits
                        (bit-shift-left 1)
                        dec
                        (bit-and carry))
                   (conj result (bit-shift-right carry (- n-carry to-bits)))))
          (let [merged-n-bits (+ n-carry from-bits) ;; consume case
                merged (bit-or (bit-shift-left carry from-bits) value)
                next-n-bits (- merged-n-bits to-bits)]
            (println "***2222" merged-n-bits merged next-n-bits)
            (if (>= merged-n-bits to-bits)
              (recur more-values
                     next-n-bits
                     (->> (- merged-n-bits to-bits)
                          (bit-shift-left 1)
                          dec
                          (bit-and merged))
                     (conj result (bit-shift-right merged next-n-bits)))
              (recur more-values;; merged-n-bits < to-bits
                     merged-n-bits
                     merged
                     result)))))
      result)))

#_
(defn convert-bits [data from-bits to-bits padding?]
  (loop [[value & more-values] data n-carry 0 carry 0 result []]
    (println "***" value n-carry carry result)
    (if (or value (pos? n-carry))
      (do (when (and value (or (neg? value) (pos? (bit-shift-right value from-bits))))
            (throw (ex-info "Invalid data - does not fit into from-bits" {:data value})))
          (if (<= to-bits n-carry) ;; don't need to consume 'value'
            (let [next-n-bits (- n-carry to-bits)]
              (println "***111" n-carry carry)
              (recur (cons value more-values)
                     next-n-bits
                     (->> next-n-bits
                          (bit-shift-left 1)
                          dec
                          (bit-and carry))
                     (conj result (bit-shift-right carry (- n-carry to-bits)))))
            (let [merged-n-bits (+ n-carry (if value from-bits 0)) ;; consume case
                  merged (bit-or (bit-shift-left carry from-bits) (or value 0))
                  next-n-bits (- merged-n-bits to-bits)]
              (println "***2222" merged-n-bits merged next-n-bits)
              (if (>= merged-n-bits to-bits)
                (recur more-values
                       next-n-bits
                       (->> (- merged-n-bits to-bits)
                            (bit-shift-left 1)
                            dec
                            (bit-and merged))
                       (conj result (bit-shift-right merged next-n-bits)))
                (recur more-values;; merged-n-bits < to-bits
                       merged-n-bits
                       merged
                       result)))))
      result)))


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

#_(defn n-bits->m-bits [data from-bits to-bits]
    (letfn [(process-carry [carry]
              (println "**** ENTER process-carry" carry)
              (loop [{:keys [data size] :as bit-data} carry result []]
                (println "**** process-carry" size to-bits)
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
            (println "**** Loop" data-to-add next-carry)
            (recur more-values next-carry (into result data-to-add)))
          (let [[carry-data left-bit-data] (process-carry carry)]
            (println "**** Final" carry-data left-bit-data)
            (->> left-bit-data
                 :size
                 (- to-bits)
                 (bit-shift-left (:data left-bit-data)) ;; padding
                 (conj carry-data)
                 (into result)))))))

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
  (convert-bits [2r11010101 2r01010101] 8 5 false)
  (convert-bits [2r11110000 2r10101010] 8 5 false)
  (convert-bits [2r10011001 2r11100011] 8 5 false)
  (convert-bits [2r00010101 2r11001100] 8 5 false)
  (convert-bits [2r01101001 2r10011010] 8 5 false))
