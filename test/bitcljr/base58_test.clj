(ns bitclojr.base58-test
  (:require [clojure.test :refer :all]
            [bitclojr.base58 :refer :all]
            [bitclojr.util :refer :all]
            [buddy.core.codecs :as codecs]
            [clojure.string :as str]))

(defonce simple-test-data [[""
                            ""
                            "3QJmnh"]
                           ["hello"
                            "Cn8eVZg"
                            "2L5B5yqsVG8Vt"]
                           ["The quick brown fox jumps over the lazy dog"
                            "7DdiPPYtxLjCD3wA1po2rvZHTDYjkZYiEtazrfiwJcwnKCizhGFhBGHeRdx"
                            "hgrbmYjTAB9gMSwZ6bUP86rvvhPkJzRkcqkmdZXXPyQCbXzuSLGmEDgmK4iSfhDR"]])

(deftest simple-cases
  (doseq [[in-str expect1 expect2] simple-test-data]
    (testing (str "Simple case: " in-str)
      (is (= expect1 (encode in-str :str)))
      (is (= expect2 (encode-check in-str :str)))
      (is (= in-str (-> in-str (encode :str) (decode :str))))
      (is (= in-str (-> in-str (encode-check :str) (decode-check :str)))))))

(deftest leading-zero-cases
  (testing "Leading zeros"
    (is (= "11ZiCa" (encode "\000\000abc" :str)))
    (is (= "\000\000abc" (-> "\000\000abc" (encode :str) (decode :str))))
    (is (= "114h3c8kNw2W" (encode-check "\000\000abc" :str)))
    (is (= "\000\000abc" (-> "\000\000abc" (encode-check :str) (decode-check :str))))
    (is (= "1111" (encode "\000\000\000\000" :str)))
    (is (= "\000\000\000\000" (-> "\000\000\000\000" (encode :str) (decode :str))))
    (is (= "11114bdQda" (encode-check "\000\000\000\000" :str)))
    (is (= "\000\000\000\000" (-> "\000\000\000\000" (encode-check :str) (decode-check :str))))))

(deftest boundary-values
  (testing "Boundary values"
    (is (= "5Q" (-> [0xff] byte-array (encode :bytes))))
    (is (= [0xff] (-> [0xff] byte-array (encode :bytes) (decode :bytes) (#(mapv signed->unsigned %)))))
    (is (= "VrZDWwe" (-> [0xff] byte-array (encode-check :bytes))))
    (is (= [0xff] (-> [0xff] byte-array (encode-check :bytes) (decode-check :bytes) (#(mapv signed->unsigned %)))))))

(deftest alternating-bytes
  (testing "Alternating bytes"
    (is (= "7X4BGT" (-> [0xff 0 0xff 0] byte-array (encode :bytes))))
    (is (= [0xff 0 0xff 0] (-> [0xff 0 0xff 0] byte-array (encode :bytes) (decode :bytes) (#(mapv signed->unsigned %)))))
    (is (= "jeriXwYhkjo" (-> [0xff 0 0xff 0] byte-array (encode-check :bytes))))
    (is (= [0xff 0 0xff 0] (-> [0xff 0 0xff 0] byte-array (encode-check :bytes) (decode-check :bytes)  (#(mapv signed->unsigned %)))))))

(deftest minimal-input
  (testing "Minimal input"
    (is (= "1" (-> [0] byte-array (encode :bytes))))
    (is (= [0] (-> [0] byte-array (encode :bytes) (decode :bytes) vec)))
    (is (= "1Wh4bh" (-> [0] byte-array (encode-check :bytes))))
    (is (= [0] (-> [0] byte-array (encode-check :bytes) (decode-check :bytes) vec)))
    (is (= "2g" (encode "a" :str)))
    (is (= "a" (-> (encode "a" :str) (decode :str))))
    (is (= "a" (-> (encode-check "a" :str) (decode-check :str))))))

(deftest long-input
  (testing "Long input"
    (is (= "6WTZQi2RjWvxih1by9eH5fc53DNk6pzuGrfQGwUa91HMti4Gx59sEaVLFu7H6vxi5SHKk3q2nz9fFwQFuj6bAoPDMhQs1AnPHzwruxYQbsWvW14Y1Gy81ZpcxCDgWx51ocMmQ5iZrS8esu38LkV7UEvL8Z4mMG5rJKAVnCFgkibD3ZvhhTBm5p7yPR1GEY7qXRqixKpnscWXj1BqVPYyJHZnTCqgz6pm3SwqZ97m4ZUdGWupua75YNwdKpaqf68Sra9StxYmHFof5tW7eZC7kCVs6LwBszrBP5ytUbgf5ANvwCVUVpE3jjDHxUvVwsktUjWNyZy6Tu85TkMeM7NKdD9YEUANDMCq43mb85M4MLuo8WtyHK5WzBUTpnbScRvunY6AodR8ABGwzeL91tSrHt88XtebtbFua6uA9sMHKM1xSJKRy9yGvKRkCb5udYGihHDN8YsX3F67JwyUbchj3YMQg4aqeFgFHzaTBJSDhqyC4j6BnB5p98V9yPJ8eqkN4qkSRAthk6uUm62mdm18F8CsKrVjbhKyvvnm54xbHSs9XrRE6aBbcxMBwwY8er3qAVXeWEYHxyz1AWCGXXrqZGMyJ21ytLvbt8Aab9gMbALRsnA1wX6YMfh4q8Vz7aBo5YGWwoY7Sy4tnjcgWGXW8pLkEyxTF8PVP1x9zbCURHRP8i3HXwahErXg58SCWsDfEjQpcqixQibk3SY9mZdpMBSrRUD6BzJCsDoQ12kVdsimLUeLJVEUsExHzbABxF6d3Mq9nnMMHuroYke5EgF3n52KVD5ej5hoyUGrszAhdTxD2TFc1WdWDubhXnS5kbUfJrTxqHFqcD4Wyn8KHqwTe8mbUSpNFPa13FfyRfhVJvFGmgcqPQ4coiZan7eMRvKgghcQPUrDvwKwqXzrmovYGQAjLjpaB4Mt7iqTyR4V1DoGpZ57g7noBqGXy9BihzTc42w2ubtTMjuAvVh5AQ9D399LCM7R9DYEsZNYQbSqoG8YfF7M2G4QtCi7ZLsyMxXD3ahrxrG3u1xwTEgk3mSFgogae5A1GvsntP4bjWQvW5avxUDB3aVPqmacEZjsWZNKKRwtruGymRqtE1RNr4ZqmxTA2o1mxDrMezJFJ9kFW274ethXe1CN4fU2e5DnCGceCew71GxSD7NXqFHXrLa7tF9WNqXR7WLMS7GJYqZFxSgRQ6nUmKot9YqTJYGEcZb1FAiZtnXVbAFTBPPrzoX4Dy27bRTPgpUvnE24WAms6saFyYwbPCVWvnQdF1SyLLKdbEsg69uBdQwV6KdPii5UUT5ug2jKNNMa7DpQnXVbVkHmHv1SjKKKASrde7zixeAb52haAygWYmPSFQpUawg1zY"
           (->> \a (repeat 1000) (apply str) (#(encode % :str)))))
    (is (= "d3RMgEwxWzZ5Tkpi3WaWuLWVZBqYYgjwwoCvfCULWBErTUepnkbmGoo4w8HmxaEo9R4SnDV31S7hc8q967uQPgpWQ7rnnzqF4Zr7yxBoWLKSCXjmEmeTe3i4jMESbQtSwAZmmi81WS1v9chGbq68Z4GsepTnbx7RAKGuzM3mrQQkp7eAe5Cm6vaxtE5PC3c9ScjzVMv3Frxj4fF3tAikNUQgavdHQxfJDrT2bmZnxdhCoUG8GRhXgVLaMfKPSpGsig4yz9bC4QVrhn74JPWKUmUJbDdhAwMwBSJappji16ynceNMGcMBGt2Vk6XgFz6CeJm7zqeYsArhAZC2XAj75KDVgtq4QskUPkeD11DBFpN7x2CD9uZU1tSaK5XTtjkXPghhJtUqXXBmMGJ4HqCeNYLw9MnzWvr1KngdjnzZXVUdLbnEoUeNBwQ55ny5Awsk2B5bwqqAfUqc8LKcFjzcjXSi9QgCEs5vQDKEJARB8av22bNmWtVy5LaQWfE9a2xfm3Q7EpcTWYTXcaWxUc7eCMXztwTBfZLQjgEZJYbNmsejpczQcZXWYjY1HvuBqhkq7V7J8LjJdRQG7fQLoWu96se8FhAZwaFAvP2D3YUC44XkiUB63SLWT3XnGgGh6nb1opSeuzz8ZJGQBkzGThxrwbzR4kyoAVfPP5EEwpZmpxZrjxNKMQyvNHpNBDwmn8exG9Uj2BKAAyBPDF2Jkhu91cxioroGgzZCoUck67R7R1gcGwA1ELH8AasfoeXyDrSnitptr5gpPjGBbjZGC2ADwkfFny7tunhRALEDTRvQcGi9gBJ6ZZ795tmjAee33qmEV9VTkRmTp3n9xosEkU6f7u2rEPie1MADr8rELGkXvNthnEhk1qCfN9F1HrfVrgDRBe2qAr3eYv4u6Ze4e6X2jwZPdNnsDbzD8sxhyKwyvh2zf3AnD1YCMJRSaNcJ8uvCXCim6XjyW4b6TWPinSy1VnoM6gZfaPbWyPvsxfBUfBFH9NProLPdzBPKTVxNNwoHiY2aBy4Cy4GdXP3RpMgC4VXiTpWEHUuTyLanpvWVmGh8h76KzAfjnKb1YH3Rv1YHXAaGWrn3esvHMdTpdiEnTCUb65yoxaYH9mWEh834frc8vfb7BmdPEUJaqnT3zQC4U9umV8JUJKonWFYY8KABd8cQ31mKZsajMVbAsa7tqChir2TXeog8owS9a4bEPBKZp9hnW3ufUoXqo4vAiSaVsdQihZfohe2QuyvDHZb8fordYPuJH2NTNEQNHvx3qtafw5YpwBzEJGiQWjqkBSxArTLJgWxLGA7Mfe6pYWZfH7eNQD33nAYqFUT4omDH4GFdbMxg7H4Wx81prgL9UcmECmpPLGB"
           (encode-check (->> \a (repeat 1000) (apply str)) :str)))))
