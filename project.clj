(defproject clj-money "1.0.0-SNAPSHOT"
  :description "Accounting application written in Clojure for the web"
  :url "http://money.herokuapp.com"
  :license {:name "Eclipse Public License v1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [ch.qos.logback/logback-classic "1.1.3"]
                 [org.clojure/java.jdbc "0.6.1"]
                 [org.postgresql/postgresql "9.4-1201-jdbc41"]
                 [honeysql "0.8.0"]
                 [compojure "1.4.0"]
                 [ring/ring-jetty-adapter "1.4.0"]
                 [hiccup "1.0.5"]
                 [environ "1.0.0"]
                 [com.cemerick/friend "0.2.3"]
                 [ragtime "0.6.3"]
                 [prismatic/schema "1.1.3"]
                 [clj-factory "0.2.1"]
                 [faker "0.2.2"]]
  :min-lein-version "2.0.0"
  :plugins [[environ/environ.lein "0.3.1"]]
  :hooks [environ.leiningen.hooks]
  :uberjar-name "clj-money-standalone.jar"
  :aliases {"migrate" ["run" "-m" "clj-money.db/migrate"]
            "rollback" ["run" "-m" "clj-money.db/rollback"]}
  :profiles {:production {:env {:production true}}})
