(ns bitclojr.segwit-addr-test
  (:require [clojure.test :refer :all]
            [bitclojr.segwit-addr :refer :all]
            [clojure.string :as str]))

(defonce +valid-addr-hex+
  [["BC1QW508D6QEJXTDG4Y5R3ZARVARY0C5XW7KV8F3T4"
    "0014751e76e8199196d454941c45d1b3a323f1433bd6"]
   ["tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7"
    "00201863143c14c5166804bd19203356da136c985678cd4d27a1b8c6329604903262"]
   ["bcrt1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqc8gma6"
    "512079be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"]
   ["tb1qqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesrxh6hy"
    "0020000000c4a5cad46221b2a187905e5266362b99d5e91c6ce24d165dab93e86433"]
   ["bcrt1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq3xueyj"
    "00200000000000000000000000000000000000000000000000000000000000000000"]
   ["bcrt1qft5p2uhsdcdc3l2ua4ap5qqfg4pjaqlp250x7us7a8qqhrxrxfsqseac85"
    "00204ae81572f06e1b88fd5ced7a1a000945432e83e1551e6f721ee9c00b8cc33260"]])

(defonce +invalid-addr+
  [["tc1qw508d6qejxtdg4y5r3zarvary0c5xw7kg3g4ty" "Mismatched decoded HRP"]
   ["bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t5" "Verifying checksum failed"]
   ["BC13W508D6QEJXTDG4Y5R3ZARVARY0C5XW7KN40WF2" "Invalid witness version" "Mismatched decoded HRP"]
   ["bc1rw5uspcuh" "Invalid data size" "Mismatched decoded HRP"]
   ["bc10w508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7kw5rljs90"
    "Invalid data size" "Mismatched decoded HRP"]
   ["BC1QR508D6QEJXTDG4Y5R3ZARVARYV98GJ9P" "Invalid witness program length"
    "Mismatched decoded HRP"]
   ["tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sL5k7" 
    "Invalid mixed lowercase and uppercase"]
   ["bc1zw508d6qejxtdg4y5r3zarvaryvqyzf3du" "Invalid enocding & witness version"
    "Mismatched decoded HRP"]
   ["tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3pjxtptv"
    "Mismatched decoded HRP" "Invalid witness program length"]
   ["bc1gmk9yu" "Invalid empty data"]
   ["bc1pw2knldczhudzzydsns4lree0fafdfn4j4nw0e5xx82lhpfvuxmtqwl4cdu"
    "Invalid enocding & witness version" "Mismatched decoded HRP"]])

(defonce +invalid-addr-enc+ 
  [["BC" 0 20 "Invalid mixed lowercase and uppercase"]
   ["bc" 0 21 "Invalid witness program length"]
   ["bc" 17 32 "Invalid witness version"]
   ["bc" 1 1 "Invalid data size"]
   ["bc" 16 41 "Invalid data size"]])


(deftest valid-addresses
  (testing "Valid segwit addresses"
    (doseq [[address script] +valid-addr-hex+]
      (let [addr (str/lower-case address)
            hrp (-> addr (str/split #"1") first)
            [wit-ver wit-prog] (decode hrp addr)]
        ;; FIXME: segwit_scriptpubkey(...)
        (is (= addr (encode hrp wit-ver wit-prog)))))))

(deftest invalid-addresses
  (testing "Invalid addesses"
    (doseq [[address err-msg1 err-msg2] +invalid-addr+]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            (re-pattern err-msg1)
                            (decode "bc" address)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            (re-pattern (or err-msg2 err-msg1))
                            (decode "tb" address))))))

(deftest invalid-addresses-enc
  (testing "Invalid addesses - encoding failures"
    (doseq [[hrp version len err-msg] +invalid-addr-enc+]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            (re-pattern err-msg)
                            (encode hrp version (repeat len 0)))))))
