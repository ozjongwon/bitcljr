(ns wallet.bip39
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;;;
;;; Read the orignal wordlist files from 'resources'
;;;
(defn load-wordlists [filenames]
  (into {}
        (for [filename filenames]
          [(-> filename
               (io/file)
               (.getName)
               (clojure.string/replace #"\.txt$" "")
               keyword)

           (with-open [reader (io/reader filename)]
             (vec (line-seq reader)))])))

(defn list-txt-files [folder-path]
  (let [folder (io/file folder-path)]
    (when (.isDirectory folder)
      (->> (.listFiles folder)
           (filter #(.isFile %))       ;; Keep only files
           (filter #(-> % .getName (clojure.string/ends-with? ".txt"))) ;; Keep .txt files
           (map #(.getAbsolutePath %)))))) ;; Return absolute paths for each file

(defonce +word-list+ (->> "resources"
                          list-txt-files
                          load-wordlists))

;;;
;;; Decoding Xhing QR result
;;;

(defn decode-standard-qr [seed-str lang-key]
  (for [i (range (int (/ (count seed-str) 4)))
        :let [idx0 (* i 4)
              idx1 (+ idx0 4)]]
    (get-in +word-list+ [lang-key (Integer/parseInt (subs seed-str idx0 idx1))])))

(defn remove-first-n-bits [n num num-bits]
  ;; get num-of-bits, shift left, dec, shift-right, bit-and
  (-> (bit-shift-left 1 num-bits)
      (dec)
      (bit-shift-right n)
      (bit-and num)))

(defn sha256 [byte-array]
  (.digest (java.security.MessageDigest/getInstance "SHA-256") byte-array))

(defn align-byte-array [barray]
  (let [aligned (loop [nibble (bit-and (second barray) 0x0f) ;; drop first byte and nibble
                       [b & more-bytes] (nthrest barray 2)
                       result []]
                  (if (nil? b)
                    result
                    (let [byte (bit-and b 0xff)]
                      (if (and (= nibble 0) (= byte 0xec))
                        result
                        (recur (bit-and byte 0x0f)
                               more-bytes
                               (conj result (bit-or (bit-shift-left nibble 4)
                                                    (bit-shift-right byte 4))))))))]
    (->> aligned
         byte-array
         sha256
         (map #(bit-and % 0xff))
         (into aligned))))

(defn extract-11-bits-dynamic [bytearray len]
  (let [bytes (align-byte-array bytearray)
        bit-length 11
        total-bits (* 8 len)]
    (loop [[byte & more-bytes] bytes
           carry 0
           carry-bits 0
           result []
           count len]
      (if (< count 0)
        result
        (let [combined (bit-or (bit-shift-left carry 8) (bit-and byte 0xff)) ; Merge carry and current byte
              n-available-bits (+ carry-bits 8)] ; Total available bits
          (if (>= n-available-bits bit-length)
            ;; If we have enough bits to extract an 11-bit value
            (let [shift-amount (- n-available-bits bit-length) ; Calculate how many bits to shift to extract the 11 bits
                  extracted-bits (bit-and (bit-shift-right combined shift-amount) 0x7FF) ; Extract the 11 bit
                  leftover-value (remove-first-n-bits bit-length combined n-available-bits)] ; Get remaining bits
              (recur more-bytes
                     leftover-value
                     shift-amount
                     (conj result extracted-bits)
                     (dec count))) ; Add the 11 bits to the result
            ;; Not enough bits yet, so we accumulate more from the next byte
            (recur more-bytes
                   combined
                   n-available-bits
                   result
                   (dec count))))))))

(defn decode-compact-qr [seed-bytes len lang-key]
  (assert (zero? (mod len 4)))
  (map #(get-in +word-list+ [lang-key %]) (extract-11-bits-dynamic seed-bytes len)))

(defn decode-seed-qr
  ([scan-result]
   (decode-seed-qr scan-result :english))
  ([{:keys [raw str]} lang-key]
   (let [byte0 (first raw)
         byte1 (second raw)
         mode (case (bit-shift-right (bit-and byte0 0xff) 4)
                0x0 :terminator
                0x1 :numeric
                0x2 :alphanumeric
                0x3 :structured-append
                0x4 :byte
                0x5 :fnc1-first-position
                0x7 :eci
                0x8 :kanji
                0x9 :fnc1-second-position
                0xd :hanzi)
         len (bit-or (bit-shift-left (bit-and byte0 0x0f) 4)
                     (bit-shift-right (bit-and byte1 0xf0) 4))]
     (case mode
       :numeric (decode-standard-qr str lang-key)
       :byte (decode-compact-qr raw len lang-key)))))

;;(decode-seed-qr "175603411269118717760390053917171617011703680513069916681107050611591746089620340812129017111526")
