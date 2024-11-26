(ns wallet.networks
  (:require [clojure.java.io :as io]
            [buddy.core.hash :as hash]
            [buddy.core.kdf :as kdf]
            [buddy.core.codecs :as codecs]
            [buddy.core.mac :as mac]
            [wallet.base58 :as b58])
  (:import [org.bouncycastle.crypto.params ECPrivateKeyParameters ECDomainParameters]
           [org.bouncycastle.crypto.ec CustomNamedCurves]
           [org.bouncycastle.asn1.sec SECNamedCurves]
           [org.bouncycastle.math.ec ECPoint]))

(defonce +networks+
  {"main" {"name" "Mainnet"
           "wif" (byte-array [0x80])
           "p2pkh" (byte-array [0x00])
           "p2sh" (byte-array [0x05])
           "bech32" "bc"
           "xprv" (byte-array [0x04 0x88 0xad 0xe4])
           "xpub" (byte-array [0x04 0x88 0xb2 0x1e])
           "yprv" (byte-array [0x04 0x9d 0x78 0x78])
           "zprv" (byte-array [0x04 0xb2 0x43 0x0c])
           "Yprv" (byte-array [0x02 0x95 0xb0 0x05])
           "Zprv" (byte-array [0x02 0xaa 0x7a 0x99])
           "ypub" (byte-array [0x04 0x9d 0x7c 0xb2])
           "zpub" (byte-array [0x04 0xb2 0x47 0x46])
           "Ypub" (byte-array [0x02 0x95 0xb4 0x3f])
           "Zpub" (byte-array [0x02 0xaa 0x7e 0xd3])
           "bip32" 0 ;;const(0) ; coin type for bip32 derivation
           }
   "test" {"name" "Testnet"
           "wif" (byte-array [0xEF])
           "p2pkh" (byte-array [0x6F])
           "p2sh" (byte-array [0xC4])
           "bech32" "tb"
           "xprv" (byte-array [0x04 0x35 0x83 0x94])
           "xpub" (byte-array [0x04 0x35 0x87 0xcf])
           "yprv" (byte-array [0x04 0x4a 0x4e 0x28])
           "zprv" (byte-array [0x04 0x5f 0x18 0xbc])
           "Yprv" (byte-array [0x02 0x42 0x85 0xb5])
           "Zprv" (byte-array [0x02 0x57 0x50 0x48])
           "ypub" (byte-array [0x04 0x4a 0x52 0x62])
           "zpub" (byte-array [0x04 0x5f 0x1c 0xf6])
           "Ypub" (byte-array [0x02 0x42 0x89 0xef])
           "Zpub" (byte-array [0x02 0x57 0x54 0x83])
           "bip32" 1 ;;const(1)
           }
   "regtest" {"name" "Regtest"
              "wif" (byte-array [0xEF])
              "p2pkh" (byte-array [0x6F])
              "p2sh" (byte-array [0xC4])
              "bech32" "bcrt"
              "xprv" (byte-array [0x04 0x35 0x83 0x94])
              "xpub" (byte-array [0x04 0x35 0x87 0xcf])
              "yprv" (byte-array [0x04 0x4a 0x4e 0x28])
              "zprv" (byte-array [0x04 0x5f 0x18 0xbc])
              "Yprv" (byte-array [0x02 0x42 0x85 0xb5])
              "Zprv" (byte-array [0x02 0x57 0x50 0x48])
              "ypub" (byte-array [0x04 0x4a 0x52 0x62])
              "zpub" (byte-array [0x04 0x5f 0x1c 0xf6])
              "Ypub" (byte-array [0x02 0x42 0x89 0xef])
              "Zpub" (byte-array [0x02 0x57 0x54 0x83])
              "bip32" 1 ;; const(1)
              }
   "signet" {"name" "Signet"
             "wif" (byte-array [0xEF])
             "p2pkh" (byte-array [0x6F])
             "p2sh" (byte-array [0xC4])
             "bech32" "tb"
             "xprv" (byte-array [0x04 0x35 0x83 0x94])
             "xpub" (byte-array [0x04 0x35 0x87 0xcf])
             "yprv" (byte-array [0x04 0x4a 0x4e 0x28])
             "zprv" (byte-array [0x04 0x5f 0x18 0xbc])
             "Yprv" (byte-array [0x02 0x42 0x85 0xb5])
             "Zprv" (byte-array [0x02 0x57 0x50 0x48])
             "ypub" (byte-array [0x04 0x4a 0x52 0x62])
             "zpub" (byte-array [0x04 0x5f 0x1c 0xf6])
             "Ypub" (byte-array [0x02 0x42 0x89 0xef])
             "Zpub" (byte-array [0x02 0x57 0x54 0x83])
             "bip32" 1 ;; const(1)
             }})
