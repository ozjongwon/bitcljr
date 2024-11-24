(ns wallet.qr
  (:require [clojure.java.io :as io]
            [wallet.base58 :as b58])
  (:import [com.github.sarxos.webcam Webcam]
           [com.google.zxing MultiFormatReader BinaryBitmap]
           [com.google.zxing.client.j2se BufferedImageLuminanceSource]
           [com.google.zxing.common HybridBinarizer BitMatrix]
           [com.google.zxing.qrcode QRCodeReader]
           [com.google.zxing.qrcode.decoder Version]
           [javax.swing JFrame JLabel ImageIcon]
           [java.awt.image BufferedImage]
           [java.awt Dimension]
           [java.security MessageDigest]))

;;;
;;; QR reading
;;;

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
        (.getRawBytes result)
        #_
        {:version "???"
         :raw (.getRawBytes result)
         :str (.getText result)
         :num-bits (.getNumBits result)
         :meta (.getResultMetadata result)}
        ;; result (.decode reader bitmap) ;; Decode the QR code
        ;; metadata (.getResultMetadata result) ;; Get the metadata
        ;; byte-segments (.get metadata ResultMetadataType/BYTE_SEGMENTS)
        #_
        {:type (.getBarcodeFormat result)
         :value (.getText result)

         :num-bits (.getNumBits result)}))
    (catch Exception e
      (when-not (instance? com.google.zxing.NotFoundException e)
        (println ">>>" e))
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

  (if-let [webcam (init-webcam)]
    (let [window (create-preview-window)]
      (try
        (.open webcam)
        (println "Scanning for QR code... (Press Ctrl+C to stop)")
        (loop [image (.getImage webcam)]
          (update-preview window image)
          (when (.isVisible (:frame window))
            (if-let [result (decode-qr-code image)]
              (do
                (println "QR Code detected! Content:" result)
                result)
              (do
                (Thread/sleep 100)       ; Small delay to prevent maxing out CPU
                (recur (.getImage webcam))))))
        (finally
          (when (.isOpen webcam)
            (.close webcam))
          (cleanup-window window))))
    (println "No webcam found!")))
