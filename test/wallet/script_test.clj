(ns wallet.script-test
  (:require [clojure.test :refer :all]
            [wallet.bip32 :as b32]
            [wallet.script :refer :all]))

(deftest script-test
  (testing "from address"
    (let [privkey (b32/make-hd-private-key (byte-array (repeat 32 0x11)) nil nil nil nil nil)]
      (doseq [script-fn [p2pkh p2sh p2wpkh p2wsh p2tr]]
        (let [script (script-fn privkey)]
          (is (=  (:data script) (-> script address address->script-pubkey))))))))
