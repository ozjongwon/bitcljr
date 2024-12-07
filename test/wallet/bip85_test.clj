(ns wallet.bip85-test
  (:require [clojure.test :refer :all]
            [wallet.bip85 :refer :all]
            ;; [wallet.base58 :as b58]
            [wallet.bip32 :as b32]
            [wallet.bip39 :as b39]
            [clojure.string :as str]
            [buddy.core.codecs :as codecs]))

(defonce +root-key+
  (b32/decode-hd-key "xprv9s21ZrQH143K2LBWUUQRFXhucrQqBpKdRRxNVq2zBqsx8HVqFk2uYo8kmbaLLHRdqtQpUm98uKfu3vca1LqdGhUtyoFnCNkfmXRyPXLjbKb"))

(defonce +bip39-vectors+
  [{:lang :english
    :expected "girl mad pet galaxy egg matter matrix prison refuse sense ordinary nose"
    :index 0 }
   {:lang :english
    :expected "near account window bike charge season chef number sketch tomorrow excuse sniff circle vital hockey outdoor supply token"
    :index 0 }
   {:lang :english
    :expected "puppy ocean match cereal symbol another shed magic wrap hammer bulb intact gadget divorce twin tonight reason outdoor destroy simple truth cigar social volcano"
    :index 0 }])

(defonce +wif-vectors+
  [{:index 0 :expected "Kzyv4uF39d4Jrw2W7UryTHwZr1zQVNk4dAFyqE6BuMrMh1Za7uhp"}])

(defonce +xprv-vectors+
  [{:index 0 :expected "xprv9s21ZrQH143K2srSbCSg4m4kLvPMzcWydgmKEnMmoZUurYuBuYG46c6P71UGXMzmriLzCCBvKQWBUv3vPB3m1SATMhp3uEjXHJ42jFg7myX"}])

(defonce +xprv-hex+
  [{:num-bytes 64 :index 0 :expected "492db4698cf3b73a5a24998aa3e9d7fa96275d85724a91e71aa2d645442f878555d078fd1f1f67e368976f04137b1f7a0d19232136ca50c44614af72b5582a5c"}])

(deftest bip85-test
  (testing "Test BIP39"
    (doseq [{:keys [lang expected index]} +bip39-vectors+]
      (binding [b39/*lang-key* lang]
        (is (= (derive-mnemonic +root-key+ lang (-> expected (str/split #" ") count) index)
               expected)))))

  (testing "Test WIF"
    (doseq [{:keys [index expected]} +wif-vectors+]
      (is (= (->  +root-key+
                  (derive-wif index))
             expected))))

  (testing "Test xprv"
    (doseq [{:keys [index expected]} +xprv-vectors+]
      (is (= (derive-xprv +root-key+ index)
             expected))))

  (testing "Test hex"
    (doseq [{:keys [index expected]} +xprv-hex+]
      (is (= (derive-hex +root-key+ index)
             expected)))))
