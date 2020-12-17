(defproject clj-money "1.0.0-SNAPSHOT"
  :description "Accounting application written in Clojure for the web"
  :url "http://money.herokuapp.com"
  :license {:name "Eclipse Public License v1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0" :exclusions [org.clojure/tools.reader]]
                 [org.clojure/tools.logging "0.4.0" :exclusions [org.clojure/tools.reader]]
                 [org.clojure/core.async "0.4.474" :exclusions [org.clojure/tools.reader]]
                 [org.clojure/tools.cli "0.3.7" :exclusions [org.clojure/tools.reader]]
                 [org.clojure/tools.reader "1.1.0"]
                 [org.clojure/data.xml "0.2.0-alpha6"]
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
                 [honeysql "0.9.10" :exclusions [org.clojure/spec.alpha
                                                 org.clojure/clojure
                                                 org.clojure/core.specs.alpha
                                                 org.clojure/tools.reader]]
                 [clj-time "0.14.3" :exclusions [org.clojure/tools.reader]]
                 [compojure "1.6.1" :exclusions [org.clojure/tools.reader]]
                 [ring/ring-core "1.8.0" :exclusions [ring/ring-codec]]
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
                 [reagent-forms "0.5.41"]
                 [reagent-utils "0.3.1"]
                 [org.clojure/clojurescript "1.10.238" :exclusions [org.clojure/tools.reader]]
                 [com.google.guava/guava "22.0" :exclusions [com.google.code.findbugs/jsr305
                                                             org.clojure/tools.reader]]
                 [clojure-guava "0.0.8" :exclusions [org.clojure/clojure
                                                     com.google.guava/guava
                                                     org.clojure/tools.reader]]
                 [secretary "1.2.3" :exclusions [com.google.javascript/closure-compiler
                                                 org.clojure/tools.reader]]
                 [venantius/accountant "0.2.4" :exclusions [com.google.javascript/closure-compiler
                                                            org.clojure/tools.reader]]
                 [closure-clj "0.1.2" :exclusions [com.google.javascript/closure-compiler
                                                   org.clojure/tools.reader]]
                 [environ "1.1.0" :exclusions [org.clojure/tools.reader]]
                 [ragtime "0.7.2" :exclusions [org.clojure/tools.reader]]
                 [clj-factory "0.2.1" :exclusions [org.clojure/tools.reader]]
                 [digest "1.4.8" :exclusions [org.clojure/tools.reader]]
                 [faker "0.3.2" :exclusions [org.clojure/tools.reader]]
                 [com.draines/postal "2.0.2" :exclusions [org.clojure/tools.reader]]
                 [com.andrewmcveigh/cljs-time "0.5.2"]
                 [buddy/buddy-sign "3.1.0" :exclusions [com.fasterxml.jackson.dataformat/jackson-dataformat-smile
                                                        com.fasterxml.jackson.dataformat/jackson-dataformat-cbor
                                                        cheshire
                                                        commons-codec
                                                        com.fasterxml.jackson.core/jackson-core]]
                 [buddy/buddy-hashers "1.4.0" :exclusions [com.fasterxml.jackson.dataformat/jackson-dataformat-smile
                                                           com.fasterxml.jackson.dataformat/jackson-dataformat-cbor
                                                           cheshire
                                                           commons-codec
                                                           com.fasterxml.jackson.core/jackson-core]]
                 [org.mindrot/jbcrypt "0.3m"]
                 [co.deps/ring-etag-middleware "0.2.1"]
                 [camel-snake-kebab "0.4.1"]
                 [stowaway "0.1.8" :exclusions [org.clojure/spec.alpha
                                                org.clojure/clojure
                                                org.clojure/core.specs.alpha
                                                org.clojure/tools.logging]]]
  :min-lein-version "2.0.0"
  :plugins [[lein-environ "1.1.0" :exclusions [org.clojure/tools.reader]]
            [lein-cljsbuild "1.1.6" :exclusions [org.clojure/tools.reader]]
            [lein-figwheel "0.5.16"]]
  :hooks []
  :uberjar-name "clj-money-standalone.jar"
  :main clj-money.web.server
  :aot [clj-money.web.server]
  :clean-targets ^{:protect false} [:target-path
                                    [:cljsbuild :builds :app :compiler :output-dir]
                                    [:cljsbuild :builds :app :compiler :output-to]]
  :source-paths ["src/clj" "src/cljc"]
  :resource-paths ["resources" "target/cljsbuild"]

  :minify-assets {:assets
                  {"resources/public/css/clj-money.min.css" "resources/public/css/clj-money.css"}}

  :cljsbuild {:builds [{:id :production
                        :source-paths ["src/cljs" "src/cljc"]
                        :compiler {:output-to  "target/cljsbuild/public/js/prod/app.js"
                                   :output-dir "target/cljsbuild/public/js/prod"
                                   :source-map "target/cljsbuild/public/js/prod/app.js.map"
                                   :optimizations :advanced
                                   :pretty-print false}}
                       {:id :development
                        :figwheel true
                        :source-paths ["src/cljs" "src/cljc"]
                        :compiler {:main "clj-money.core"
                                   :asset-path "/js/app"
                                   :output-to  "resources/public/js/app/main.js"
                                   :output-dir "resources/public/js/app"
                                   :source-map true
                                   :optimizations :none
                                   :pretty-print true}}]}

  :aliases {"migrate"                       ["run" "-m" "clj-money.db/migrate"]
            "rollback"                      ["run" "-m" "clj-money.db/rollback"]
            "remigrate"                     ["run" "-m" "clj-money.db/remigrate"]
            "partition"                     ["run" "-m" "clj-money.db/create-partitions"]
            "check-trans"                   ["run" "-m" "clj-money.db/check-transaction-balances"]
            "chunk-file"                    ["run" "-m" "clj-money.import.gnucash/chunk-file"]
            "seed"                          ["run" "-m" "clj-money.seed/seed"]
            "generate-transactions"         ["run" "-m" "clj-money.seed/generate-transactions"]
            "recalc"                        ["run" "-m" "clj-money.tasks/recalc"]
            "migrate-account"               ["run" "-m" "clj-money.tasks/migrate-account"]
            "update-commodity-price-ranges" ["run" "-m" "clj-money.tasks/update-commodity-price-ranges"]}

  :jvm-opts ["-Duser.timezone=UTC"]
  :profiles {:production {:env {:production true}}
            :dev [:project/dev :profiles/dev]
            :test [:project/test :profiles/test]
            :profiles/dev {}
            :profiles/test {}
            :project/dev {:env
                          {:db "postgresql://app_user:please01@localhost/money_development"
                           :partition-period "year"
                           :show-error-messages? "true"
                           :site-protocol "http"
                           :site-host "lvh.me:5000"}}
            :project/test {:dependencies [[ring/ring-mock "0.4.0"]
                                          [peridot "0.5.2"]]
                           :env
                           {:db "postgresql://app_user:please01@localhost/money_test" 
                            :partition-period "year"
                            :mailer-host "testmailer.com"
                            :mailer-from "no-reply@clj-money.com"
                            :application-name "clj-money"
                            :show-error-messages? "true"
                            :detailed-import-logging? "false"
                            :google-client-id "google-id"
                            :google-client-secret "google-client-secret"
                            :secret "9c3931112e73122ab46bf6fc0c40e72490b72b444b857dec35abc07056d3e867d952664eae62ba1db8d94089834ee2fd9fc989d6af8c7bd21fcfb6371bde27d3"
                            :site-protocol "https"
                            :site-host "www.mymoney.com"}}
            :uberjar {:prep-tasks ["compile" ["cljsbuild" "once"]]}})
