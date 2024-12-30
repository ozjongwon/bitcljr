(ns wallet.fountain
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [medley.core :as mc]
            [clj-cbor.core :as cbor]
            [buddy.core.hash :as hash]
            [buddy.core.codecs :as codecs]))

;;https://github.com/BlockchainCommons/URKit/blob/master/Sources/URKit/Fountain/Xoshiro256.swift

(defprotocol Xoshiro256PRNG
  (next [this] [this size])
  (jump [this])
  (long-jump [this])
  (next-state [this]))

(defonce +mask64+ (BigInteger. (apply str (repeat 16 \f)) 16))

(defn- rotate-left [x k]
  (let [k (mod k 64)]
    (.or (.shiftLeft x k)
         (.shiftRight x (- 64 k)))))

(defrecord Xoshiro256** [state] ;; 256 bit(8 * 32) seed
  Xoshiro256PRNG
  (next [this]
    (next this :64))
  (next [this size]
    (case size
      :32 "32bit int"
      :64 (let [[s0 s1 s2 s3] state
                result (-> s1
                           (.multiply (biginteger 5))
                           (rotate-left 7)
                           (.multiply (biginteger 9))
                           (.and +mask64+))
                t (.shiftLeft s1 17)
                s2 (.xor s2 s0)
                s3 (.xor s3 s1)]
            [result
             (->> [(.xor s0 s3) (.xor s1 s2)
                   (.xor s2 t) (rotate-left s3 45)]
                  (mapv #(.and % +mask64+))
                  (assoc this :state ))]))))

(defn make-xoshiro256** [seed]
  (condp = (type seed)
    String (-> seed hash/sha256 recur)
    byte/1 (->> seed (partition 8) (mapv #(BigInteger. 1 (byte-array %))) recur)
    (->Xoshiro256** seed)))
