(defproject clj-money "1.0.0-SNAPSHOT"
  :description "Accounting application written in Clojure for the web"
  :url "http://money.herokuapp.com"
  :license {:name "Eclipse Public License v1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.4" :exclusions [org.clojure/tools.reader]]
                 [org.clojure/tools.logging "1.3.0" :exclusions [org.clojure/tools.reader]]
                 [org.clojure/core.async "1.6.681" :exclusions [org.clojure/tools.reader]]
                 [org.clojure/tools.cli "1.0.206" :exclusions [org.clojure/tools.reader]]
                 [org.clojure/tools.reader "1.3.4"]
                 [org.clojure/data.xml "0.2.0-alpha6"]
                 [clj-http "3.9.0" :exclusions [org.clojure/tools.reader]]
                 [cheshire "5.8.0" :exclusions [org.clojure/tools.reader]]
                 [com.github.kyleburton/clj-xpath "1.4.11" :exclusions [org.clojure/tools.reader]]
                 [ch.qos.logback/logback-classic "1.2.3" :exclusions [org.clojure/tools.reader]]
                 [org.clojure/java.jdbc "0.7.11" :exclusions [org.clojure/tools.reader]]
                 [org.postgresql/postgresql "42.7.4" :exclusions [org.clojure/tools.reader]]
                 [clj-postgresql "0.7.0" :exclusions [org.slf4j/slf4j-api
                                                      org.postgresql/postgresql
                                                      org.clojure/tools.reader]]
                 [honeysql "0.9.10" :exclusions [org.clojure/spec.alpha
                                                 org.clojure/clojure
                                                 org.clojure/core.specs.alpha
                                                 org.clojure/tools.reader]]
                 [org.threeten/threeten-extra "1.8.0"]
                 [clojure.java-time "1.4.2"]
                 [compojure "1.6.1" :exclusions [joda-time clj-time org.clojure/tools.reader]]
                 [org.eclipse.jetty/jetty-util "9.4.36.v20210114" :exclusions [org.slf4j/slf4j-api]]
                 [org.eclipse.jetty/jetty-io "9.4.36.v20210114" :exclusions [org.slf4j/slf4j-api]]
                 [org.eclipse.jetty/jetty-server "9.4.36.v20210114" :exclusions [org.slf4j/slf4j-api]]
                 [ring/ring-core "1.8.0" :exclusions [ring/ring-codec]]
                 [ring/ring-jetty-adapter "1.6.3" :exclusions [joda-time clj-time org.clojure/tools.reader]]
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
                 [org.clojure/clojurescript "1.11.4" :exclusions [org.clojure/tools.reader]]
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
                 [yogthos/config "1.2.0" :exclusions [org.clojure/spec.alpha
                                                      org.clojure/clojure
                                                      org.clojure/core.specs.alpha]]
                 [ragtime "0.7.2" :exclusions [org.clojure/tools.reader]]
                 [clj-factory "0.2.1" :exclusions [org.clojure/tools.reader]]
                 [digest "1.4.8" :exclusions [org.clojure/tools.reader]]
                 [faker "0.3.2" :exclusions [org.clojure/tools.reader]]
                 [com.draines/postal "2.0.2" :exclusions [org.clojure/tools.reader]]
                 [com.andrewmcveigh/cljs-time "0.5.2"]
                 [crypto-random "1.2.1" :exclusions [commons-codec]] ; added to clarify dependencies
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
                 [co.deps/ring-etag-middleware "0.2.1" :exclusions [joda-time clj-time]]
                 [camel-snake-kebab "0.4.3"]
                 [com.github.dgknght/app-lib "0.3.14" :exclusions [com.cognitect/transit-java com.google.protobuf/protobuf-java com.google.errorprone/error_prone_annotations org.clojure/google-closure-library-third-party com.fasterxml.jackson.dataformat/jackson-dataformat-smile ring/ring-core org.apache.httpcomponents/httpasyncclient com.fasterxml.jackson.dataformat/jackson-dataformat-cbor org.eclipse.jetty/jetty-http ring/ring-codec org.apache.httpcomponents/httpmime org.eclipse.jetty/jetty-io org.eclipse.jetty/jetty-server com.google.javascript/closure-compiler-externs com.cognitect/transit-cljs cljs-http camel-snake-kebab cheshire noencore commons-io commons-codec clj-http joda-time clj-time com.google.jsinterop/jsinterop-annotations org.apache.httpcomponents/httpclient com.google.code.findbugs/jsr305 com.cognitect/transit-clj org.clojure/google-closure-library org.apache.httpcomponents/httpcore-nio ring/ring-servlet com.google.javascript/closure-compiler-unshaded org.clojure/clojurescript org.apache.httpcomponents/httpclient-cache org.apache.httpcomponents/httpcore ring/ring-jetty-adapter com.fasterxml.jackson.core/jackson-core]]
                 [lambdaisland/uri "1.4.54"]
                 [stowaway "0.1.20" :exclusions [org.clojure/spec.alpha org.clojure/clojure potemkin org.clojure/core.specs.alpha org.clojure/tools.logging]]]
  :repl-options {:init-ns clj-money.repl
                 :welcome (println "Welcome to better money management!")}
  :min-lein-version "2.0.0"
  :plugins [[lein-cljfmt "0.7.0"]]
  :hooks []
  :uberjar-name "clj-money-standalone.jar"
  :aot [clj-money.web.server]
  :clean-targets ^{:protect false} [:target-path]
  :source-paths ["src"]
  :aliases {"migrate"                       ["run" "-m" "clj-money.db/migrate"]
            "rollback"                      ["run" "-m" "clj-money.db/rollback"]
            "remigrate"                     ["run" "-m" "clj-money.db/remigrate"]
            "partition"                     ["run" "-m" "clj-money.db/create-partitions"]
            "check-trans"                   ["run" "-m" "clj-money.db/check-transaction-balances"]
            "chunk-file"                    ["run" "-m" "clj-money.import.gnucash/chunk-file"]
            "seed"                          ["run" "-m" "clj-money.seed/seed"]
            "generate-transactions"         ["run" "-m" "clj-money.seed/generate-transactions"]
            "sass"                          ["run" "-m" "clj-money.tasks/compile-sass"]
            "recalc"                        ["run" "-m" "clj-money.tasks/recalc"]
            "migrate-account"               ["run" "-m" "clj-money.tasks/migrate-account"]
            "export-user-tags"              ["run" "-m" "clj-money.tasks/export-user-tags"]
            "import-user-tags"              ["run" "-m" "clj-money.tasks/import-user-tags"]
            "update-commodity-price-ranges" ["run" "-m" "clj-money.tasks/update-commodity-price-ranges"]
            "fig:build"                     ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]
            "fig:min"                       ["run" "-m" "figwheel.main" "-O" "advanced" "-bo" "dev"]
            "fig:test"                      ["run" "-m" "figwheel.main" "-co" "test.cljs.edn" "-m" "clj-money.test-runner"]}

  :jvm-opts ["-Duser.timezone=UTC"]
  :profiles {:test {:dependencies [[ring/ring-mock "0.4.0"]
                                   [peridot "0.5.2"]]
                    :resource-paths ^:replace ["env/test" "resources" "target"]}
             :dev {:dependencies [[com.bhauman/figwheel-main "0.2.17" :exclusions [ring/ring-anti-forgery ring/ring-devel com.google.errorprone/error_prone_annotations ring/ring-core org.eclipse.jetty/jetty-http ring/ring-codec org.eclipse.jetty/jetty-io com.google.guava/guava org.eclipse.jetty/jetty-server ring commons-codec joda-time clj-time org.slf4j/slf4j-api]]
                                  [org.slf4j/slf4j-nop "1.7.30" :exclusions [org.slf4j/slf4j-api]]
                                  [com.bhauman/rebel-readline-cljs "0.1.4"]]
                   :resource-paths ^:replace ["env/dev" "resources" "target"]}
             :uberjar {:prep-tasks ["compile"
                                    "sass"]}})
