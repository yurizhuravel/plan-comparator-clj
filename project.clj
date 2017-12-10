(defproject comparator-clj "0.1.0-SNAPSHOT"
  :description "A simple command-line comparator"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.json "0.2.6"]
                 [com.taoensso/timbre "4.10.0"]]
  :main ^:skip-aot comparator-clj.core)
