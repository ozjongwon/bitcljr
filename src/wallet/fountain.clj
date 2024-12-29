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

;; (defn- signed->unsigned-long [l]
;;   (let [two-64 (.shiftLeft BigInteger/ONE 64)
;;         b (BigInteger/valueOf l)]
;;     (if (neg? (.signum b))
;;       (.add b two-64)
;;       b)))

(defn- rotate-left [x k]
  (.or (.shiftLeft x k)
       (.shiftRight x (- 64 k))))

(defrecord Xoshiro256** [state] ;; 256 bit(8 * 32) seed
  Xoshiro256PRNG
  (next [this]
    (next this :64))
  (next [this size]
    (case size
      :32 "32bit int"
      :64 (let [result (-> (second state)
                           (.multiply (biginteger 5))
                           (rotate-left 7)
                           (.multiply (biginteger 9)))
                t (.shiftLeft (second state) 17)
                s2 (.xor (nth state 2) (nth state 0))
                s3 (.xor (nth state 3) (nth state 1))
                s1 (.xor (nth state 1) s2)
                s0 (.xor (nth state 0) s3)]
            [result
             (assoc this :state [s0 s1 (.xor s2 t) (rotate-left s3 45)])])
      :float "<= 0 <= 1")))

;; (defn- bytes->longs [bytes]
;;   (let [lb (.asLongBuffer (java.nio.ByteBuffer/wrap bytes))
;;         la (long-array (.capacity lb))]
;;     (.get lb la)
;;     la))

;; (defn make-xoshiro256** [seed]
;;   (condp = (type seed)
;;     String (recur (hash/sha256 seed))
;;     byte/1 (recur (bytes->longs seed))
;;     long/1 (->> seed
;;                 (mapv (comp biginteger signed->unsigned-long))
;;                 ->Xoshiro256**)))

(defn make-xoshiro256** [seed]
  (condp = (type seed)
    String (-> seed hash/sha256 recur)
    byte/1 (->> seed (partition 8) (mapv #(BigInteger. 1 (byte-array %))) recur)
    (->Xoshiro256** seed)))
