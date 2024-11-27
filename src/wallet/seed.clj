(ns wallet.seed
  (:require [wallet.qr :as qr]
            [wallet.bip39 :as b39]
            [wallet.bip32 :as b32]))

(defn qr->seed
  ([]
   (qr->seed :english))
  ([lang-key]
   (-> (qr/scan-qr-code-continuously)
       (b39/decode-seed-qr lang-key)
       (b39/mnemonics->seed)
       (b32/seed->hd-key))))
