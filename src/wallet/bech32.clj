(ns wallet.bech32
  (:require [clojure.string :as str]
            [buddy.core.hash :as hash]
            [buddy.core.codecs :as codecs]))

(defonce +charset+ "qpzry9x8gf2tvdw0s3jn54khce6mua7l")

(defonce +encoding-consts+ {:bech32 0x01
                            :bech32m 0x2bc830a3}) ;; taproot

(defn polymod [values]
  (let [generator [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3]]
    (reduce (fn [checksum val]
              (let [top7-bits (bit-shift-right checksum 25)]
                (reduce (fn [chck i]
                          (if (zero? (bit-and (bit-shift-right top7-bits i) 1))
                            chck
                            (bit-xor (get generator i) chck)))
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

(defn verify-checksum [hrp data enc-k]
  (= (polymod (into (expand-human-readable-part hrp) data))
     (get +encoding-consts+ enc-k)))

(defn create-checksum [hrp data enc-k]
  (let [pmod (-> `[~@(expand-human-readable-part hrp) ~@data 0 0 0 0 0 0]
                 polymod
                 (bit-xor (get +encoding-consts+ enc-k)))]
    (reduce (fn [acc i]
              (conj acc (bit-shift-right pmod (* (- 5 i) 5))))
            []
            (range 6))))

(defn encode [hrp data enc-k]
  (->> enc-k
       (create-checksum hrp data)
       (into data)
       (map #(get +charset+ %))
       (into (conj hrp \1))
       (apply str)))

(defn probe-case [ch-or-str]
  (let [target (str ch-or-str)
        maybe-lower (str/lower-case target)
        maybe-upper (str/upper-case target)]
    (cond (= target maybe-lower maybe-upper) :na
          (= target maybe-lower) :lower
          (= target maybe-upper) :upper
          :else :mixed)))

(defn decode [bech-str enc-k]
  (when-not (every? #(<= 33 (int %) 126) bech-str)
    (throw (ex-info "Invalid character out of range" {:bech-str  bech-str})))

  (let [lower-bech-str (str/lower-case bech-str)
        pos1 (str/last-index-of bech-str "1")
        bech-size (count bech-str)]
    (when-not (or (= lower-bech-str bech-str)
                  (= (str/upper-case bech-str) bech-str))
      (throw (ex-info "Invalid mixed lowercase and uppercase" {:bech-str bech-str})))

    (when-not (and (int? pos1)
                   (<= 1 pos1 (+ pos1 7) bech-size 90))
      (throw (ex-info "Invalid sperator position '1'" {:position-1 pos1
                                                       :bech-size bech-size})))
    (let [hrp (subs lower-bech-str 0 pos1)
          decoded-data (mapv #(str/index-of +charset+ %) (subs lower-bech-str (inc pos1)))]
      (when-not (every? identity decoded-data)
        (throw (ex-info "Invalid characters in 'data'" {:position-1 pos1
                                                        :data (subs bech-str (inc pos1))})))
      (when-not (verify-checksum hrp decoded-data enc-k)
        (throw (ex-info "Verifying checksum failed" {:hrp hrp
                                                     :data (subs bech-str (inc pos1))
                                                     :enc enc-k})))
      [(subs lower-bech-str 0 pos1) ;; hrp
       (subvec decoded-data 0 (- (count decoded-data) 6))])))
