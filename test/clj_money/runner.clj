(ns clj-money.runner
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.cli :refer [parse-opts]]
            [java-time.api :as t]
            [eftest.runner :refer [find-tests run-tests]]
            [clj-money.config :refer [env]]
            [clj-money.db.sql.tasks :as sql]
            [clj-money.db.sql.partitioning :refer [create-partition-tables]]))

(defn- init-sql-db
  [config]
  (sql/create config :silent true)
  (sql/migrate config)
  (create-partition-tables config
                           (t/local-date 2015 1 1)
                           (t/local-date 2017 12 31)
                           {:silent true}))

(defn- init-sql-dbs []
  (let [config (get-in env [:db :strategies :sql])
        db-count (.availableProcessors (Runtime/getRuntime))
        configs (map (comp #(assoc %
                                   :user (env :sql-adm-user)
                                   :password (env :sql-adm-password))
                           #(update-in config [:dbname] str "_" %))
                     (range 0 db-count))]
    (doseq [c configs]
      (init-sql-db c))))

(def ^:private eftest-options
  [["-s" "--strategy STRATEGY" "The storage strategy"
    :parse-fn keyword
    :validate [#{:sql :datomic-peer} "Unrecognized storage strategy"]]
   ["-m" "--multithread MULTITHREAD"
    "The strategy for dividing work into threads. (Default namespaces)"
    :id :multithread?
    :default :namespaces
    :parse-fn keyword
    :validate [#{:namespaces :vars "Unrecognized multithread strategy"}]]
   ["-v" "--verbose"
    "Indicates whether or not to render test output for passing tests. (Default false)"
    :parse-fn not
    :id :capture-output?]
   ["-h" "--help" "Show this help message"]])

(def ^:private not-multi-threaded?
  (complement :multi-threaded))

(defn- match-strategy?
  [selector]
  (when selector
    #(= selector
        (-> % meta :strategy))))

(defn- match-meta?
  [{:keys [strategy]}]
  (->> [not-multi-threaded?
        (match-strategy? strategy)]
       (filter identity)
       (apply every-pred)))

(defn- pass-thru
  [m]
  (pprint m)
  m)

(defn- run?
  [options]
  (comp (match-meta? options)
        pass-thru
        meta))

(defn- write-help
  [{:keys [errors summary]}]
  (when (seq errors)
    (println "ERRORS:")
    (doseq [e errors] (println "  - " e))
    (println ""))
  (println "lein ptest [OPTIONS] [ARGUMENTS]")
  (println "")
  (println "OPTIONS:")
  (print summary)
  (println ""))

(defn- show-help?
  [{:keys [errors arguments options]}]
  (or (seq errors)
            (:help options)
            (some #(= "help" %)
                  arguments)))

(defn- eftest*
  [{:keys [options
           arguments]}]
  (init-sql-dbs)
  (->> (or (seq arguments) ["test"])
       (mapcat find-tests)
       (filter (run? options))
       run-tests options))

(defn eftest
  [& args]
  (let [parsed (parse-opts args eftest-options)]
    (if (show-help? parsed)
      (write-help parsed)
      (eftest* parsed))))
