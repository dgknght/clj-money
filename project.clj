(defproject clj-money "1.0.0-SNAPSHOT"
  :description "Accounting application written in Clojure for the web"
  :url "http://money.herokuapp.com"
  :license {:name "Eclipse Public License v1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/core.async "0.3.443"]
                 [org.clojure/tools.cli "0.3.5"]
                 [slingshot "0.12.2"]
                 [clj-http "3.5.0"]
                 [cheshire "5.7.1"]
                 [com.github.kyleburton/clj-xpath "1.4.11"]
                 [ch.qos.logback/logback-classic "1.1.3"]
                 [org.clojure/java.jdbc "0.6.1"]
                 [org.postgresql/postgresql "9.4-1201-jdbc41"]
                 [clj-postgresql "0.7.0"]
                 [honeysql "0.8.0"]
                 [clj-time "0.12.0"]
                 [compojure "1.4.0"]
                 [ring/ring-jetty-adapter "1.4.0"]
                 [ring/ring-codec "1.0.1"]
                 [ring/ring-json "0.4.0"]
                 [ring/ring-anti-forgery "1.1.0"]
                 [hiccup "1.0.5"]
                 [selmer "1.11.2"]
                 [environ "1.1.0"]
                 [com.cemerick/friend "0.2.3"]
                 [ragtime "0.6.3"]
                 [clj-factory "0.2.1"]
                 [digest "1.4.5"]
                 [faker "0.2.2"]
                 [com.draines/postal "2.0.2"]]
  :min-lein-version "2.0.0"
  :plugins [[lein-environ "1.1.0"]
            [lein-deps-tree "0.1.2"]]
  :hooks []
  :uberjar-name "clj-money-standalone.jar"
  :main clj-money.web.server
  :aot [clj-money.web.server]
  :aliases {"migrate"               ["run" "-m" "clj-money.db/migrate"]
            "rollback"              ["run" "-m" "clj-money.db/rollback"]
            "partition"             ["run" "-m" "clj-money.db/create-partitions"]
            "chunk-file"            ["run" "-m" "clj-money.import.gnucash/chunk-file"]
            "seed"                  ["run" "-m" "clj-money.seed/seed"]
            "generate-transactions" ["run" "-m" "clj-money.seed/generate-transactions"]}
  :jvm-opts ["-Duser.timezone=UTC"]
  :profiles {:production {:env {:production true}}})
