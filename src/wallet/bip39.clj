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

(defn byte-array->nbits-int [byte-array n]
  (loop [[byte & more-bytes] byte-array bit-mask 0xff result []]
    ))


(defn parse-qr-data
  "Parses raw QR code data to return data type, data length, and data without padding.
   Assumes the input is a byte array/vector."
  [raw-bytes]
  (let [mode-indicator (bit-shift-right (first raw-bytes) 4) ;; First 4 bits determine the mode
        mode-type (case mode-indicator
                    1 :numeric
                    2 :alphanumeric
                    4 :byte
                    7 :eci
                    8 :kanji)
        length-bits (case mode-indicator
                      1 10  ;; Length field size for numeric mode
                      2 9   ;; Length field size for alphanumeric mode
                      4 8   ;; Length field size for byte mode
                      8 8   ;; Length field size for kanji mode
                      0)    ;; Default
        length-bytes (Math/ceil (/ length-bits 8.0)) ;; Length field in bytes
        length-value (if (pos? length-bytes)
                       (reduce (fn [acc b] (+ (* acc 256) (bit-and b 0xFF)))
                               0
                               (take length-bytes (rest raw-bytes)))
                       0)
        data-bytes (->> (drop (+ 1 length-bytes) raw-bytes) ;; Skip mode and length
                        (take length-value)) ;; Take only actual data
        stripped-data (filter #(not (#{0xEC 0x11} %)) data-bytes)] ;; Remove padding
    {:data-type mode-type
     :data-length length-value
     :data (byte-array stripped-data)}))

;; [16, -64, -64, 100, -115, -61, -84, -82, 76, -109, 113, -16, 37, -52, 107, -54,
;;  4, -58, 3, 56, 83, 72, 0, -20, 17, -20, 17, -20, 17, -20, 17, -20, 17, -20]
;; [17, -128, 11, -128, 79, -78, 104, 12, -79, -8, 76, 12, 41, 126, -38, 84, -124,
;;  -66, -101, 5, 13, -104, 26, -103, 90, -10, -112, -55, -102, 121, -119, -104,
;;  89, -102, -46, 36, -80, -104, 102, 0, -86, 12, 0, -20, 17, -20, 17, -20, 17,
;;  -20, 17, -20, 17, -20, 17]
;; [65, 5, -69, -39, -41, 26, -114, -57, -103, 8, 49, -81, -13, 89, -44, 38, 84,
;;  80, -20]
;; [66, 0, -25, 75, 100, 16, 127, -108, -52, 12, -49, -82, 106, 19, -36, -66, -61,
;;  102, 33, 84, -2, -58, 126, 14, 0, -103, -100, 7, -119, 37, -105, -47, -112,
;;  -96]


;; Note that for some QR decoders, zxing in particular, may return more data than the compact seed itself:
;; the data type, the data length, and padding. For example, the zxing result from 12-word CompactSeedQRs
;; in low error correction mode will start with 41 0 (that specifies the binary data format and says
;; the length of the data is 16 bytes) and end with 0 ec (unused byte and a half). Similarly,
;; 24-word CompactSeedQRs will start with 42 0 (binary, data length is 32 bytes) and end with 0
;; (unused half a byte).


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
                      (clojure.pprint/cl-format true "~%>> ~4,'0b + ~8,'0b => ~b"
                                                nibble
                                                byte
                                                (bit-or (bit-shift-left nibble 4)
                                                        (bit-shift-right byte 4)))
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
        (do
          (clojure.pprint/cl-format true "~%??~8,'0b(~b) " byte carry)
          result)
        (let [combined (bit-or (bit-shift-left carry 8) (bit-and byte 0xff)) ; Merge carry and current byte
              n-available-bits (+ carry-bits 8)] ; Total available bits
          (clojure.pprint/cl-format true "~%~D: ~8,'0b(~b) => ~b" carry-bits byte carry combined)
          (if (>= n-available-bits bit-length)
            ;; If we have enough bits to extract an 11-bit value
            (let [shift-amount (- n-available-bits bit-length) ; Calculate how many bits to shift to extract the 11 bits
                  extracted-bits (bit-and (bit-shift-right combined shift-amount) 0x7FF) ; Extract the 11 bit
                  leftover-value (remove-first-n-bits bit-length combined n-available-bits)] ; Get remaining bits
              (clojure.pprint/cl-format true " -> ~b" leftover-value)
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
                   (dec count)))))))
  #_
  (let [bytes-wo-checksum (mapv #(bit-and % 0xff) bytearray)
        bytes (into bytes-wo-checksum (map #(bit-and % 0xff) (sha256 (byte-array bytes-wo-checksum))))
        bit-length 11
        total-bits (* 8 len)
        checksum-bits (/ total-bits 32)]
    (clojure.pprint/cl-format true "~%***~a <> ~a " (into [] bytes-wo-checksum) bytes)
    (loop [[byte & more-bytes] (rest bytes)
           carry (first bytes)
           carry-bits 4 ;; first nibble
           result []
           count len]
      (if (zero? count)
        (do
          (clojure.pprint/cl-format true "~%??~8,'0b(~b) " byte carry)
          result)
        (let [combined (bit-or (bit-shift-left carry 8) (bit-and byte 0xff)) ; Merge carry and current byte
              n-available-bits (+ carry-bits 8)] ; Total available bits
          (clojure.pprint/cl-format true "~%~D: ~8,'0b(~b) => ~b" carry-bits byte carry combined)
          (if (>= n-available-bits bit-length)
            ;; If we have enough bits to extract an 11-bit value
            (let [shift-amount (- n-available-bits bit-length) ; Calculate how many bits to shift to extract the 11 bits
                  extracted-bits (bit-and (bit-shift-right combined shift-amount) 0x7FF) ; Extract the 11 bit
                  leftover-value (remove-first-n-bits bit-length combined n-available-bits)] ; Get remaining bits
              (clojure.pprint/cl-format true " -> ~b" leftover-value)
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

;; 4: Byte mode indicator
;; 0f: length of 15 byte
;; d15001...: your 15 bytes of data
;; ec11 is just padding

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
