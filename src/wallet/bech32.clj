(ns wallet.bech32
  (:require [buddy.core.hash :as hash]
            [buddy.core.codecs :as codecs]))

(defonce +charset+ "qpzry9x8gf2tvdw0s3jn54khce6mua7l")

(defonce +encoding-consts+ {:bech32 0x01
                            :bech32m 0x2bc830a3}) ;; taproot

#_
(defn polymod [values]
  (let [generator [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3]]
    (loop [checksum 1 [val & more-vals] values]
      (let [top7-bits (bit-shift-right checksum 25)]
        (if val
          (recur (loop [[i & next-is] (range 5) checksum (-> checksum
                                                             (bit-and 0x1ffffff) ;; remove top 7 bits
                                                             (bit-shift-left 5) ;; 5 bit spaces for val
                                                             (bit-xor val)) ]
                   (if (and i (bit-and (bit-shift-right top7-bits i) 1))
                     (recur next-is (bit-xor (get generator i) checksum))
                     checksum))
                 more-vals)
          checksum)))))

#_
(defn polymod [values]
  (let [generator [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3]]
    (loop [checksum 1 [val & more-vals] values]
      (let [top7-bits (bit-shift-right checksum 25)]
        (if val
          (reduce (fn [chck i]
                    (if (and i (bit-and (bit-shift-right top7-bits i) 1))
                      (bit-xor (get generator i) chck)
                      chck))
                  (-> checksum
                      (bit-and 0x1ffffff) ;; remove top 7 bits
                      (bit-shift-left 5) ;; 5 bit spaces for val
                      (bit-xor val))
                  (range 5))
          checksum)))))

(defn polymod [values]
  (let [generator [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3]]
    (reduce (fn [checksum val]
              (let [top7-bits (bit-shift-right checksum 25)]
                (reduce (fn [chck i]
                          (if (and i (bit-and (bit-shift-right top7-bits i) 1))
                            (bit-xor (get generator i) chck)
                            chck))
                        (-> checksum
                            (bit-and 0x1ffffff) ;; remove top 7 bits
                            (bit-shift-left 5)  ;; 5 bit spaces for val
                            (bit-xor val))
                        (range 5))))
            1
            values)))

(defn expand-human-readable-part [s]
  (loop [[ch & more-ch] s front [] back []]
    (if ch
      (let [i (int ch)]
        (recur more-ch
               (conj front (bit-shift-right i 5))
               (conj back (bit-and i 31))))
      `[~@front 0 ~@back])))






















(defn- polymod
  "Bech32 generator polynomial calculation"
  [values]
  (reduce
   (fn [chk v]
     (let [top (bit-shift-right chk 25)
           chk (bit-and (bit-shift-left (bit-and chk 0x1FFFFFF) 5) (bit-xor v top))]
       (reduce
        (fn [c i]
          (if (bit-test top i)
            (bit-xor c (nth [0x3b6a57b2 0x26508e6d 0x1ea119fa 0x3d4233dd 0x2a1462b3] i))
            c))
        chk
        (range 5))))
   1
   values))

(defn- eight-bit->five-bit [data]
  "Convert 8-bit to 5-bit encoding"
  [data]
  (let [converted (transient [])]
    (loop [acc 0
           bits 0
           input (seq data)
           converted converted]
      (if (empty? input)
        (persistent! converted)
        (let [value (first input)
              acc (bit-or (bit-shift-left acc 8) value)
              bits (+ bits 8)]
          (if (>= bits 5)
            (recur
             (bit-shift-right acc (- bits 5))
             (- bits 5)
             (rest input)
             (conj! converted
                    (bit-and
                     (bit-shift-right acc (- bits 5))
                     0x1F)))
            (recur
             acc
             bits
             (rest input)
             converted)))))))

#_
(defn- eight-bit->five-bit
  "Convert 8-bit to 5-bit encoding"
  [data]
  (let [converted (transient [])]
    (loop [acc 0
           bits 0
           input (seq data)
           converted converted]
      (if (empty? input)
        (persistent! converted)
        (let [value (first input)
              acc (bit-or (bit-shift-left acc 8) value)
              bits (+ bits 8)]
          (if (>= bits 5)
            (recur
             (bit-shift-right acc (- bits 5))
             (- bits 5)
             (rest input)
             (conj! converted
                    (bit-and
                     (bit-shift-right acc (- bits 5))
                     0x1F)))
            (recur
             acc
             bits
             (rest input)
             converted)))))))

(defn encode-bech32
  "Encode data to bech32"
  [hrp data]
  (let [combined
        (concat
         (map int hrp)
         [0]
         (eight-bit->five-bit data))]
    (let [checksum
          (-> (polymod
               (concat combined [0 0 0 0 0 0]))
              (bit-xor 1))]
      (str
       hrp
       "1"
       (apply str
              (map
               #(nth bech32-charset
                     (bit-and
                      (bit-shift-right checksum (- 25 (* 5 %)))
                      0x1F))
               (range 6)))))))

(defn decode-bech32
  "Decode bech32 string"
  [bech32-string]
  (let [lower (.toLowerCase bech32-string)
        [hrp rest-str] (clojure.string/split lower #"1")
        data-part (subs rest-str 0 (- (count rest-str) 6))
        checksum-part (subs rest-str (- (count rest-str) 6))]
    {:hrp hrp
     :data (map #(.indexOf bech32-charset (str %)) data-part)}))

(defn validate-bech32
  "Validate bech32 encoding"
  [bech32-string]
  (try
    (let [decoded (decode-bech32 bech32-string)]
      (= bech32-string
         (encode-bech32
          (:hrp decoded)
          (map int (:data decoded)))))
    (catch Exception _ false)))

;; Bitcoin-specific bech32 conversion
(defn convert-bits
  "Convert between bit sizes"
  [data from-bits to-bits pad?]
  (let [acc (transient [])
        bits 0
        n 0]
    (reduce
     (fn [[acc bits n] value]
       (let [bits (+ bits from-bits)
             n (bit-or (bit-shift-left n from-bits) value)
             acc
             (if (>= bits to-bits)
               (conj! acc (bit-and (bit-shift-right n (- bits to-bits)) (dec (bit-shift-left 1 to-bits))))
               acc)
             bits (if (>= bits to-bits) (- bits to-bits) bits)]
         [acc bits n]))
     [acc bits n]
     data)))

;; Example usage for Bitcoin addresses
(defn bitcoin-address-to-bech32
  "Convert Bitcoin address to bech32"
  [address]
  (let [decoded (decode-bech32 address)]
    {:hrp (:hrp decoded)
     :version (first (:data decoded))
     :address (:data decoded)}))
