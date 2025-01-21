(defproject bitclojr "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.clojure/core.async "1.6.681"]
                 [org.flatland/ordered "1.15.12"]
                 [com.github.sarxos/webcam-capture "0.3.12"]
                 [com.google.zxing/core "3.5.3"]
                 [com.google.zxing/javase "3.5.2"]
                 [buddy/buddy-hashers "2.0.167"]
                 [dev.weavejester/medley "1.8.1"]
                 [com.sparrowwallet/hummingbird "1.7.4"]
                 [camel-snake-kebab "0.4.3"]
                 [cljfx "1.9.3"]
                 [cljfx/css "1.1.0"]
                 [org.clojure/data.json "2.5.0"]
                 [clj-http "3.13.0"]
                 [clojure.java-time "1.4.3"]

                 ;; [org.clj-commons/gloss "0.3.6"]
                 ;; [ring/ring-core "1.13.0"]
                 ;; [ring/ring-json "0.5.1"]
                 ;; [aleph "0.8.1"]
                 ]
  ;; pushd ~/Work/aleph; rm -rf ~/.m2/repository/aleph/; lein pom; lein jar; lein install
  :jvm-opts ["-Djdk.attach.allowAttachSelf=true"]
  :repl-options {:init-ns bitclojr.core})
