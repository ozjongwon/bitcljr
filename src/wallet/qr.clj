(ns qr-scanner.core
  (:require [clojure.java.io :as io])
  (:import [com.github.sarxos.webcam Webcam]
           [com.google.zxing MultiFormatReader BinaryBitmap]
           [com.google.zxing.client.j2se BufferedImageLuminanceSource]
           [com.google.zxing.common HybridBinarizer]
           [javax.swing JFrame JLabel ImageIcon]
           [java.awt.image BufferedImage]
           [java.awt Dimension]))

(def scanning-active (atom true))
(def current-frame (atom nil))

(defn create-preview-window
  "Creates a preview window showing webcam feed"
  []
  (let [frame (JFrame. "QR Scanner Preview")
        label (JLabel.)]
    (.setDefaultCloseOperation frame JFrame/DISPOSE_ON_CLOSE)
    (.add frame label)
    (.setPreferredSize frame (Dimension. 640 480))
    (.pack frame)
    (.setVisible frame true)
    {:frame frame :label label}))

(defn cleanup-window
  "Properly dispose of the preview window"
  [{:keys [frame]}]
  (when frame
    (.dispose frame)))

(defn update-preview
  "Updates the preview window with current frame"
  [{:keys [label]} image]
  (when (and label image)
    (.setIcon label (ImageIcon. image))))

(defn decode-qr-code
  "Attempts to decode QR code from a buffered image"
  [^BufferedImage buffered-image]
  (try
    (let [source (BufferedImageLuminanceSource. buffered-image)
          bitmap (BinaryBitmap. (HybridBinarizer. source))
          reader (MultiFormatReader.)]
      (when-let [result (.decode reader bitmap)]
        (.getText result)))
    (catch Exception _
      nil)))

(defn init-webcam
  "Initialize and configure the webcam"
  []
  (let [webcam (Webcam/getDefault)]
    (when webcam
      (.setViewSize webcam (Dimension. 640 480))
      webcam)))

(defn scan-qr-code-continuously
  "Continuously scans for QR codes until one is found"
  []
  (println "Initializing webcam...")
  (reset! scanning-active true)  ; Reset the scanning state

  (if-let [webcam (init-webcam)]
    (let [window (create-preview-window)]
      (try
        (.open webcam)
        (println "Scanning for QR code... (Press Ctrl+C to stop)")
        (loop []
          (when @scanning-active
            (let [image (.getImage webcam)]
              (reset! current-frame image)
              (update-preview window image)
              (if-let [result (decode-qr-code image)]
                (do
                  (println "QR Code detected! Content:" result)
                  result)
                (do
                  (Thread/sleep 100)  ; Small delay to prevent maxing out CPU
                  (recur))))))
        (finally
          (reset! scanning-active false)
          (reset! current-frame nil)
          (when (.isOpen webcam)
            (.close webcam))
          (cleanup-window window))))
    (println "No webcam found!")))

(defn stop-scanning!
  "Stops the scanning process"
  []
  (reset! scanning-active false))

;; Example usage:
;; Start scanning:
;; (scan-qr-code-continuously)
;;
;; To stop scanning from another REPL thread:
;; (stop-scanning!)

;; (ns qr-scanner.core
;;   (:require [clojure.java.io :as io])
;;   (:import [com.github.sarxos.webcam Webcam]
;;            [com.google.zxing MultiFormatReader BinaryBitmap]
;;            [com.google.zxing.client.j2se BufferedImageLuminanceSource]
;;            [com.google.zxing.common HybridBinarizer]
;;            [javax.swing JFrame JLabel ImageIcon]
;;            [java.awt.image BufferedImage]
;;            [java.awt Dimension]))
