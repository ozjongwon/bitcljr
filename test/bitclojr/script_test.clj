(ns bitclojr.script-test
  (:require [clojure.test :refer :all]
            [bitclojr.bip32 :as b32]
            [bitclojr.script :refer :all]
            [bitclojr.ecc :as ecc]))

(deftest script-test
  (testing "from address"
    (let [privkey (ecc/make-private-key (byte-array (repeat 32 0x11)) nil nil nil nil nil)]
      (doseq [script-fn [p2pkh p2sh p2wpkh #_ p2wsh p2tr]]
        (let [script (script-fn privkey)]
          (is (=  (:data script) (-> script address address->script-pubkey))))))))
