(ns bitclojr.base58
  (:require [bitclojr.util :as util]
            [buddy.core.crypto :as crypto]
            [buddy.core.nonce :as nonce]
            [clojure.edn :as edn]

            [buddy.core.hash :as hash]
            [buddy.core.codecs :as codecs]
            [clojure.string :as str])
  (:import [java.util Base64 Arrays]))

;;;
;;; HW wallet - prv key neverleaves the device.
;;;
;;; master-seed + "m/44'/0'/0' => xpub & xprv
;;;

;; 2. High-Level Flow:
;; SW Wallet creates and proposes a transaction.
;; SW Wallet sends the unsigned transaction to the HW Wallet.
;; HW Wallet signs the transaction using its master private key or a derived private key.
;; HW Wallet returns the signed transaction back to the SW Wallet.
;; SW Wallet broadcasts the signed transaction to the Bitcoin network.

(defonce wallet-structure {:name "abc"
                           :derivation-path "m/44'/0'/0'/0"
                           :root-fingerprint "???"
                           :xpub "master pubkey"
                           :type "HW" ;; or "wallet', etc?
                           :script-type "p2pkh" :master-public-key "xpub..."})
;; "m/44'/0'/0'/1" (external receiving address)
;; "m/44'/0'/0'/0" (internal change address)

(edn/read-string (pr-str wallet-structure))

;;https://github.com/sparrowwallet/sparrow/blob/master/src/main/java/com/sparrowwallet/sparrow/io/Electrum.java#L405
(defn encrypt [plain password]
  (let [key (util/double-sha256 password)
        iv (nonce/random-bytes 12)]
    (->> `[~@iv ~@(crypto/encrypt (codecs/to-bytes plain)
                                  key
                                  iv
                                  {:algorithm :aes256-gcm})]
         byte-array
         (.encode (Base64/getEncoder))
         (codecs/bytes->str))))

(defn decrypt [encrypted password]
  (let [key (util/double-sha256 password)
        encrypted-bytes (-> (Base64/getDecoder)
                            (.decode encrypted))]

    (-> (crypto/decrypt (Arrays/copyOfRange encrypted-bytes 12 (count encrypted-bytes))
                        key
                        (Arrays/copyOfRange encrypted-bytes 0 12)
                        {:algorithm :aes256-gcm})
        (codecs/bytes->str))))

(comment
  (def enw (encrypt (pr-str wallet-structure) "Hello World!"))
  (edn/read-string (decrypt enw "Hello World!"))
  )
