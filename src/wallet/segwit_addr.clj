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

(defn m-bits->n-bits
  ([data from-bits to-bits]
   (m-bits->n-bits data from-bits to-bits true))
  ([data from-bits to-bits padding?]
   (letfn [(process-carry [carry]
             (loop [{:keys [data size] :as bit-data} carry result []]
               (cond (zero? size) [result (make-bit-data 0 0)]
                     (= size to-bits) [(conj result data) (make-bit-data 0 0)]
                     (< size to-bits) [result (make-bit-data data size)]
                     :else (let [[a b] (bit-split bit-data to-bits)]
                             (recur b (conj result (:data a)))))))]
     (loop [[signed-value & more-values] data
            carry (make-bit-data 0 0)
            result []]
       (if signed-value
         (let [value (bit-and signed-value 0xff)
               [data-to-add next-carry] (->> from-bits
                                             (make-bit-data value)
                                             (bit-merge carry)
                                             process-carry)]
           (recur more-values next-carry (into result data-to-add)))
         (let [[carry-data {:keys [data size]}] (process-carry carry)]
           (into result (cond (and padding? (zero? data) (pos? size)) (conj carry-data data)
                              (zero? data) carry-data
                              :else (conj carry-data (bit-shift-left data (- to-bits size)))))))))))

(comment
  (let [data (byte-array [-4, 114, 80, -94, 17, -34, -35, -57, 14, -27, -94, 115, -115, -27, -16, 120,
                          23, 53, 28, -17])]
    (println (buddy.core.codecs/bytes->hex data))
    (= (m-bits->n-bits data 8 5 true)
       [31, 17, 25, 5, 1, 8, 16, 17, 27, 27, 14, 28, 14, 3, 23, 5, 20, 9, 25, 24, 27, 25, 15, 16, 15, 0, 11, 19, 10, 7, 7, 15]))
  ;; 0xfc7250a211deddc70ee5a2738de5f07817351cef
  ;; 0xff111905ff081011fbff0efc0e03ff05f40919f8ff19ff100f000b130a07ff0f
  "1111110001110010010100001010001000010001110111101101110111000111000011101110010110100010011100111000110111100101111100000111100000010111001101010001110011101111"
  )

(comment
  (for [n [2r10101100 2r0000000100 2r11111111 2r01010101 2r00001111
           2r11111000 2r11100000 2r00000001 2r00001111]]
    (let [eight-bitv (m-bits->n-bits [n] 8 5)]
      (= [n] (m-bits->n-bits eight-bitv 5 8 false))))
  )

(defn decode [hrp addr]
  (let [[encoding bec-hrp bec-data] (bc32/decode addr)]
    (when (empty? bec-data)
      (throw (ex-info "Invalid empty data" {:address addr})))

    (when-not (= hrp bec-hrp)
      (throw (ex-info "Mismatched decoded HRP" {:hrp-expect hrp
                                                :hrp-got bec-hrp})))
    (let [witness-prog (m-bits->n-bits (subvec bec-data 1) 5 8 false)
          len (count witness-prog)
          witness-ver (get bec-data 0)]
      (when-not (<= 2 len 40)
        (throw (ex-info "Invalid data size" {:data witness-prog})))

      (condp apply [witness-ver] ;; 'witness version'
        #(< 16 %)
        (throw (ex-info "Invalid witness version"
                        {:witness-version (get witness-prog 0)}))
        #(and (zero? %) (not (contains? #{20 32} len)))
        (throw (ex-info "Invalid witness program length"
                        {:witness-program-length len}))
        #(or (and (zero? %) (not= encoding :bech32))
             (and (not (zero? %)) (not= encoding :bech32m)))
        (throw (ex-info "Invalid enocding & witness version"
                        {:encoding encoding
                         :witness-version (get witness-prog 0)}))
        :ok)
      [witness-ver witness-prog])))

(defn encode [hrp wit-ver wit-prog]
  (let [encoding (if (= wit-ver 0) :bech32 :bech32m)
        encoded (bc32/encode hrp `[~wit-ver ~@(m-bits->n-bits wit-prog 8 5)] encoding)]
    (assert (decode hrp encoded))
    encoded))
