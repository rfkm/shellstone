(defproject shellstone "0.1.0-SNAPSHOT"
  :description "An experimental programming language in which Method Shells is implemented"
  :url "https://github.com/rkworks/shellstone"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.cli "0.2.2"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.blancas/kern "0.7.0"]]
  :profiles {:dev {:dependencies [[midje "1.6.3"]
                                  [org.clojure/tools.trace "0.7.8"]]
                   :plugins [[lein-midje "3.1.1"]]}})
