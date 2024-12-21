(ns wallet.bech32-test
  (:require [clojure.test :refer :all]
            [wallet.bech32 :refer :all]
            [clojure.string :as str]))


(defonce +valid-checksum-bech32+
  ["A12UEL5L"
   "a12uel5l"
   "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs"
   "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw"
   "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j"
   "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w"
   "?1ezyfcl"])

(defonce +valid-checksum-bech32m+
  ["A1LQFN3A"
   "a1lqfn3a"
   "an83characterlonghumanreadablepartthatcontainsthetheexcludedcharactersbioandnumber11sg7hg6"
   "abcdef1l7aum6echk45nj3s0wdvt2fg8x9yrzpqzd3ryx"
   "11llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllludsr8"
   "split1checkupstagehandshakeupstreamerranterredcaperredlc445v"
   "?1v759aa"])

(def +invalid-checksum-bech32+
  [[" 1nwldj5" "Invalid character out of range"]
   [(str (char 0x7f) "1axkwrx") "Invalid character out of range"]
   [(str (char 0x80) "1eym55h") "Invalid character out of range"]
   ["an84characterslonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1569pvx"
    "Invalid sperator position '1'"]
   ["pzry9x0s0muk" "Invalid sperator position '1'"]
   ["1pzry9x0s0muk" "Invalid sperator position '1'"]
   ["x1b4n0q5v" "Invalid characters in 'data'"]
   ["li1dgmt3" "Invalid sperator position '1'"]
   [(str "de1lg7wt" (char 0xff)) "Invalid character out of range"]
   ["A1G7SGD8" "Verifying checksum failed"]
   ["10a06t8" "Invalid sperator position '1'"]
   ["1qzzfhee" "Invalid sperator position '1'"]])

(def +invalid-checksum-bech32m+
  [[" 1xj0phk" "Invalid character out of range"]
   [(str (char 0x7f) "1g6xzxy") "Invalid character out of range"]
   [(str (char 0x80) "1vctc34") "Invalid character out of range"]
   ["an84characterslonghumanreadablepartthatcontainsthetheexcludedcharactersbioandnumber11d6pts4"
    "Invalid sperator position '1'"]
   ["qyrz8wqd2c9m" "Invalid sperator position '1'"]
   ["1qyrz8wqd2c9m" "Invalid sperator position '1'"]
   ["y1b0jsk6g" "Invalid characters in 'data'"]
   ["lt1igcx5c0" "Invalid characters in 'data'"]
   ["in1muywd" "Invalid sperator position '1'"]
   ["mm1crxm3i" "Invalid characters in 'data'"]
   ["au1s5cgom" "Invalid characters in 'data'"]
   ["M1VUXWEZ" "Verifying checksum failed"]
   ["16plkw9" "Invalid sperator position '1'"]
   ["1p2gdwpf" "Invalid sperator position '1'"]])

(deftest valid-bech-checksum
  (testing "Valid bech32"
    (doseq [tv +valid-checksum-bech32+]
      (is (decode tv :bech32))))
  (testing "Valid bech32m"
    (doseq [tv +valid-checksum-bech32m+]
      (is (decode tv :bech32m)))))

(deftest invalid-bech-checksum
  (testing "Invalid bech32"
    (doseq [[tv err-msg] +invalid-checksum-bech32+]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo (re-pattern err-msg) (decode tv :bech32)))))
  (testing "Invalid bech32m"
    (doseq [[tv err-msg] +invalid-checksum-bech32m+]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo (re-pattern err-msg) (decode tv :bech32))))))
