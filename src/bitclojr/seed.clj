(ns bitclojr.seed
  (:require [bitclojr.qr :as qr]
            [bitclojr.bip39 :as b39]
            [bitclojr.bip32 :as b32]))

(defn qr->seed
  ([]
   (qr->seed :english))
  ([lang-key]
   (-> (qr/scan-qr-code-continuously)
       (b39/decode-seed-qr lang-key)
       (b39/mnemonic->seed)
       (b32/seed->hd-key))))
