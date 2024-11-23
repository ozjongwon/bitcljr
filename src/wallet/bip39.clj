(ns wallet.bip39
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

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

(defn decode-seed-qr
  ([seed-str]
   (decode-seed-qr seed-str :english))
  ([seed-str lang-key]
   (for [i (range (int (/ (count seed-str) 4)))
         :let [idx0 (* i 4)
               idx1 (+ idx0 4)]]
     (get-in +word-list+ [lang-key (Integer/parseInt (subs seed-str idx0 idx1))]))))

;;(decode-seed-qr "175603411269118717760390053917171617011703680513069916681107050611591746089620340812129017111526")
