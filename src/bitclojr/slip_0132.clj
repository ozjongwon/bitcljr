(ns bitclojr.slip-0132)

(defonce +script+path->address-type+
  ;; https://github.com/BlockchainCommons/Research/blob/91fb927d1550e0135c00e726e177780f2977e6bb/papers/bcr-2020-015-account.md
  ;; &
  ;; https://github.com/satoshilabs/slips/blob/master/slip-0132.md
  {["pkh" "m/44'/0'/0'"] "xpub"
   ["wpkh" "m/84'/0'/0'"] "zpub"
   ;; multisig
   ["wsh" "m/48'/0'/0'/2'"] "Zpub"})

(defn get-address-types [{:keys [path script] :as ur-account}]
  (get +script+path->address-type+ [script path]))

;; {:master-fingerprint "78ede2ce",
;;  :output-descriptors
;;  [{:hd-key
;;    {:chain-code
;;     "bc9473195e203536a962fa211c02a5929effb597bb7a946de48067297b29042c",
;;     :parent-fingerprint "0e144480",
;;     :key "03e0b0bb752a26ee43a2733b3fdbc6632379d5e23d20f9b3b6cd8d82ca4ec72c7c",
;;     :origin
;;     {:source-fingerprint "78ede2ce",
;;      :depth 3,
;;      :path "84'/0'/0'",
;;      :components [{:index 84} {:index 0} {:index 0}]}},
;;    :script-expressions
;;    [{:tag-value 404,
;;      :expression "wpkh",
;;      :declaring-class
;;      com.sparrowwallet.hummingbird.registry.ScriptExpression}]}]}


;; {:master-fingerprint "78ede2ce",
;;  :output-descriptors
;;  [{:hd-key
;;    {:chain-code
;;     "aeb17b220446b72ac204b677067f3a21e2a3ba9683a5afcb229d5a82cb96ab02",
;;     :parent-fingerprint "e6d8998e",
;;     :key "0244432d006c357920078df950e5a1114756c925cfd07f7c1352f20c49fabf7b45",
;;     :origin
;;     {:source-fingerprint "78ede2ce",
;;      :depth 4,
;;      :path "48'/0'/0'/2'",
;;      :components [{:index 48} {:index 0} {:index 0} {:index 2}]}},
;;    :script-expressions
;;    [{:tag-value 401,
;;      :expression "wsh",
;;      :declaring-class
;;      com.sparrowwallet.hummingbird.registry.ScriptExpression}]}]}
