(defproject clj-money "1.0.0-SNAPSHOT"
  :description "Accounting application written in Clojure for the web"
  :url "http://money.herokuapp.com"
  :license {:name "Eclipse Public License v1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.logging "0.4.0"]
                 [org.clojure/core.async "0.4.474" :exclusions [org.clojure/data.priority-map
                                                                org.clojure/core.cache]]
                 [org.clojure/tools.cli "0.3.7"]
                 [slingshot "0.12.2"]
                 [clj-http "3.9.0"]
                 [cheshire "5.8.0"]
                 [com.github.kyleburton/clj-xpath "1.4.11"]
                 [ch.qos.logback/logback-classic "1.2.3"]
                 [org.clojure/java.jdbc "0.7.6"]
                 [org.postgresql/postgresql "42.2.2"]
                 [clj-postgresql "0.7.0" :exclusions [org.slf4j/slf4j-api
                                                      org.postgresql/postgresql]]
                 [honeysql "0.9.2"]
                 [clj-time "0.14.3"]
                 [compojure "1.6.1"]
                 [ring/ring-jetty-adapter "1.6.3"]
                 [ring/ring-codec "1.1.1"]
                 [ring/ring-json "0.4.0"]
                 [ring/ring-anti-forgery "1.2.0"]
                 [hiccup "1.0.5"]
                 [selmer "1.11.7" :exclusions [joda-time com.google.javascript/clojure-compiler]]
                 [reagent "0.8.0" :exclusions [com.google.code.findbugs/jsr305]]
                 [org.clojure/clojurescript "1.10.238"]
                 [com.google.guava/guava "22.0" :exclusions [com.google.code.findbugs/jsr305]]
                 [clojure-guava "0.0.8" :exclusions [org.clojure/clojure
                                                     com.google.guava/guava]]
                 [secretary "1.2.3" :exclusions [com.google.javascript/clojure-compiler]]
                 [venantius/accountant "0.2.4" :exclusions [com.google.javascript/clojure-compiler]]
                 [closure-clj "0.1.2"]
                 [environ "1.1.0"]
                 [com.cemerick/friend "0.2.3"]
                 [ragtime "0.7.2"]
                 [clj-factory "0.2.1"]
                 [digest "1.4.8"]
                 [faker "0.3.2"]
                 [com.draines/postal "2.0.2"]]
  :min-lein-version "2.0.0"
  :plugins [[lein-environ "1.1.0"] 
            [lein-cljsbuild "1.1.7"]]
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
