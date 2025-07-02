(ns clj-money.db.datomic.tasks
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [config.core :refer [env]]
            [datomic.api :as d]))

(defn schema []
  (mapcat (comp edn/read-string
                slurp
                io/resource
                #(format "datomic/schema/%s.edn" %))
          ["model"
           "cached_price"
           "user"
           "identity"
           "entity"
           "grant"
           "commodity"
           "price"
           "account"
           "attachment"
           "transaction"
           "transaction_item"
           "lot"
           "lot_item"
           "image"
           "budget"
           "budget_item"
           "reconciliation"]))

(defn apply-schema
  ([] (apply-schema :datomic-peer))
  ([config-key]
   (let [{:as cfg
          :keys [db-name]} (get-in env [:db :strategies config-key])]
     (assert cfg (str "No datomic configuration found for " config-key))
     (apply-schema cfg
                   db-name)))
  ([{:keys [uri] :as cfg} {:keys [suppress-output?]}]
   {:pre [(:uri cfg)]}
   (try
     (log/info "creating the database...")
     (d/create-database uri)
     (log/info "database created.")
     (log/info "applying the schema...")
     (let [res @(d/transact (d/connect uri)
                            (schema))]
       (when-not suppress-output?
         (pprint {::transact-schema res})))
     (log/info "done applying the schema.")
     (catch Exception e
       (log/error e "error when applying the schema.")
       (throw e))
     (finally (d/shutdown true)))))
