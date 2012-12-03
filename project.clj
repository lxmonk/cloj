(defproject learning "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins  [[lein-swank "1.4.4"]
             [lein-iclojure "1.0"]]
  :dependencies [[org.clojure/clojure "1.5.0-alpha6"]
                 [slingshot "0.10.3"]
                 [crypto-random "1.1.0"]
                 [org.clojure/tools.nrepl "0.2.0-beta9"]
                 [clj-http "0.1.3"]
                 [org.clojure/data.json "0.1.3"]
                 [org.apache.commons/commons-email "1.2"]
                 [org.clojure/tools.trace "0.7.3"]
                 [com.novemberain/monger "1.2.0"]]
  :main learning.BitcoinCheck)
