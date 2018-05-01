(defproject clj-money "1.0.0-SNAPSHOT"
  :description "Accounting application written in Clojure for the web"
  :url "http://money.herokuapp.com"
  :license {:name "Eclipse Public License v1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0" :exclusions [org.clojure/tools.reader]]
                 [org.clojure/tools.logging "0.4.0" :exclusions [org.clojure/tools.reader]]
                 [org.clojure/core.async "0.4.474" :exclusions [org.clojure/data.priority-map
                                                                org.clojure/core.cache
                                                                org.clojure/tools.reader]]
                 [org.clojure/tools.cli "0.3.7" :exclusions [org.clojure/tools.reader]]
                 [org.clojure/tools.reader "1.1.0" :exclusions [org.clojure/tools.reader]]
                 [slingshot "0.12.2" :exclusions [org.clojure/tools.reader]]
                 [clj-http "3.9.0" :exclusions [org.clojure/tools.reader]]
                 [cheshire "5.8.0" :exclusions [org.clojure/tools.reader]]
                 [com.github.kyleburton/clj-xpath "1.4.11" :exclusions [org.clojure/tools.reader]]
                 [ch.qos.logback/logback-classic "1.2.3" :exclusions [org.clojure/tools.reader]]
                 [org.clojure/java.jdbc "0.7.6" :exclusions [org.clojure/tools.reader]]
                 [org.postgresql/postgresql "42.2.2" :exclusions [org.clojure/tools.reader]]
                 [clj-postgresql "0.7.0" :exclusions [org.slf4j/slf4j-api
                                                      org.postgresql/postgresql
                                                      org.clojure/tools.reader]]
                 [honeysql "0.9.2" :exclusions [org.clojure/tools.reader]]
                 [clj-time "0.14.3" :exclusions [org.clojure/tools.reader]]
                 [compojure "1.6.1" :exclusions [org.clojure/tools.reader]]
                 [ring/ring-jetty-adapter "1.6.3" :exclusions [org.clojure/tools.reader]]
                 [ring/ring-codec "1.1.1" :exclusions [org.clojure/tools.reader]]
                 [ring/ring-json "0.4.0" :exclusions [org.clojure/tools.reader]]
                 [ring/ring-anti-forgery "1.2.0" :exclusions [org.clojure/tools.reader]]
                 [hiccup "1.0.5" :exclusions [org.clojure/tools.reader]]
                 [cljs-http "0.1.45" :exclusions [org.clojure/tools.reader]]
                 [selmer "1.11.7" :exclusions [joda-time
                                               com.google.javascript/closure-compiler
                                               org.clojure/tools.reader]]
                 [reagent "0.8.0" :exclusions [com.google.code.findbugs/jsr305
                                               org.clojure/tools.reader]]
                 [org.clojure/clojurescript "1.10.238" :exclusions [org.clojure/tools.reader
                                                                    org.clojure/tools.reader]]
                 [com.google.guava/guava "22.0" :exclusions [com.google.code.findbugs/jsr305
                                                             org.clojure/tools.reader]]
                 [clojure-guava "0.0.8" :exclusions [org.clojure/clojure
                                                     com.google.guava/guava
                                                     org.clojure/tools.reader]]
                 [secretary "1.2.3" :exclusions [com.google.javascript/closure-compiler org.clojure/tools.reader]]
                 [venantius/accountant "0.2.4" :exclusions [com.google.javascript/closure-compiler org.clojure/tools.reader]]
                 [closure-clj "0.1.2" :exclusions [com.google.javascript/closure-compiler
                                                   org.clojure/tools.reader]]
                 [environ "1.1.0" :exclusions [org.clojure/tools.reader]]
                 [com.cemerick/friend "0.2.3" :exclusions [org.clojure/tools.reader]]
                 [ragtime "0.7.2" :exclusions [org.clojure/tools.reader]]
                 [clj-factory "0.2.1" :exclusions [org.clojure/tools.reader]]
                 [digest "1.4.8" :exclusions [org.clojure/tools.reader]]
                 [faker "0.3.2" :exclusions [org.clojure/tools.reader]]
                 [com.draines/postal "2.0.2" :exclusions [org.clojure/tools.reader]]]
  :min-lein-version "2.0.0"
  :plugins [[lein-environ "1.1.0" :exclusions [org.clojure/tools.reader]]
            [lein-cljsbuild "1.1.7" :exclusions [org.clojure/tools.reader]]]
  :hooks []
  :uberjar-name "clj-money-standalone.jar"
  :main clj-money.web.server
  :aot [clj-money.web.server]
  :clean-targets ^{:protect false} [:target-path
                                    [:cljsbuild :builds :app :compiler :output-dir]
                                    [:cljsbuild :builds :app :compiler :output-to]]
  :source-paths ["src/clj"]
  :resource-paths ["resources" "target/cljsbuild"]

  :minify-assets {:assets
                  {"resources/public/css/clj-money.min.css" "resources/public/css/clj-money.css"}}

  :cljsbuild {:builds {:min {:source-paths ["src/cljs"]
                             :compiler {:output-to  "target/cljsbuild/public/js/app.js"
                                        :output-dir "target/cljsbuild/public/js"
                                        :source-map "target/cljsbuild/public/js/app.js.map"
                                        :optimizations :advanced
                                        :pretty-print false}}
                       :app {:source-paths ["src/cljs"]
                             :compiler {:main "clj-money.core"
                                        :asset-path "js/out"
                                        :output-to  "target/cljsbuild/public/js/app.js"
                                        :output-dir "target/cljsbuild/public/js/out"
                                        :source-map true
                                        :optimizations :none
                                        :pretty-print true}}}}

  :aliases {"migrate"               ["run" "-m" "clj-money.db/migrate"]
            "rollback"              ["run" "-m" "clj-money.db/rollback"]
            "partition"             ["run" "-m" "clj-money.db/create-partitions"]
            "chunk-file"            ["run" "-m" "clj-money.import.gnucash/chunk-file"]
            "seed"                  ["run" "-m" "clj-money.seed/seed"]
            "generate-transactions" ["run" "-m" "clj-money.seed/generate-transactions"]}
  :jvm-opts ["-Duser.timezone=UTC"]
  :profiles {:production {:env {:production true}}})
