(ns clj-money.db.sql
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [next.jdbc :as jdbc]
            [next.jdbc.plan :refer [select!
                                    select-one!]]
            [next.jdbc.sql.builder :refer [for-insert
                                           for-update
                                           for-delete]]
            [stowaway.criteria :as crt]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.inflection :refer [plural]]
            [clj-money.util :as util]
            [clj-money.db :as db]
            [clj-money.db.sql.queries :refer [criteria->query]]
            [clj-money.db.sql.types :refer [temp-id?
                                            coerce-id]]))

(defmulti before-save db/type-dispatch)
(defmethod before-save :default [m] m)

(defmulti after-read db/model-type)
(defmethod after-read :default [m] m)

(defmulti prepare-criteria db/model-type)
(defmethod prepare-criteria :default [m] m)

(defmulti attributes identity)

(def ^:private infer-table-name
  (comp keyword
        plural
        util/qualifier))

(defn- insert
  [db model]
  (let [table (infer-table-name model)
        s (for-insert table
                      model
                      jdbc/snake-kebab-opts)
        result (jdbc/execute-one! db s {:return-keys [:id]})]

    ; TODO: scrub for sensitive data
    (log/debugf "database insert %s -> %s" model s)
    (get-in result [(keyword (name table) "id")])))

(defn- update
  [db model]
  {:pre [(:id model)]}
  (let [table (infer-table-name model)
        s (for-update table
                      (dissoc model :id)
                      {:id (:id model)}
                      jdbc/snake-kebab-opts)
        result (jdbc/execute-one! db s {:return-keys [:id]})]

    ; TODO: scrub sensitive data
    (log/debugf "database update %s -> %s" model s)

    (get-in result [(keyword (name table) "id")])))

(defn delete-one
  [db m]
  (let [s (for-delete (infer-table-name m)
                      {:id (:id m)} ; TODO: find the id attribute
                      {})]

    ; TODO: scrub sensitive data
    (log/debugf "database delete %s -> %s" m s)

    (jdbc/execute! db s)
    1))

(defn- put-one
  [ds [oper model]]
  (case oper
    ::db/insert (insert ds model)
    ::db/update (update ds model)
    ::db/delete (delete-one ds model)
    (throw (ex-info "Invalid operation" {:operation oper}))))

(defn- wrap-oper
  "Ensure that what we are passing on is a tuple with a database
  operation in the 1st position and a model in the second."
  [m]
  (cond
    (vector? m) m
    (:id m)     [::db/update m]
    :else       [::db/insert m]))

(defmulti resolve-temp-ids
  "In a model-specific way, replace temporary ids with proper ids after a save."
  (fn [model _id-map]
    (db/model-type model)))

(defmethod resolve-temp-ids :default
  [model _id-map]
  model)

(defn- execute-and-aggregate
  "Executes the database operation, saves the result and
  updates the id map for resolving temporary ids"
  [ds {:as result :keys [id-map]} [operator m]]
  (let [id-resolved (cond-> m
                      (seq id-map) (resolve-temp-ids id-map)
                      (temp-id? m) (dissoc :id))
        saved (put-one ds [operator id-resolved])]
    (cond-> (update-in result [:saved] conj saved)
      (temp-id? m)
      (assoc-in [:id-map (:id m)]
                saved))))

(defn- put*
  [ds models]
  (jdbc/with-transaction [tx ds]
    (->> models
         (map (comp wrap-oper
                    before-save))
         (reduce (partial execute-and-aggregate tx)
                 {:saved []
                  :id-map {}}))))

(defn- id-key
  [x]
  (when-let [target (db/model-type x)]
    (keyword (name target) "id")))

(defn- massage-ids
  "Coerces ids and appends the appropriate namespace
  to the :id key"
  [m]
  (let [k (id-key m)]
    (cond-> (crt/apply-to m #(update-in-if % [:id] coerce-id))
      k (rename-keys {:id k}))))

(defn- select*
  [ds criteria options]
  (let [query (-> criteria
                  (crt/apply-to massage-ids)
                  prepare-criteria
                  (criteria->query (assoc options
                                          :target (db/model-type criteria))))]

    ; TODO: scrub sensitive data
    (log/debugf "database select %s with options %s -> %s" criteria options query)

    (if (:count options)
      (select-one! ds
                   :record-count
                   query
                   jdbc/unqualified-snake-kebab-opts)
      (let [q (db/model-type criteria)]
        (map (comp after-read
                   #(util/qualify-keys % q :ignore #{:id}))
             (select! ds
                      (attributes q)
                      query
                      jdbc/snake-kebab-opts))))))

(defn- delete* [_ds _models])

(defn- reset*
  [ds]
  (jdbc/execute! ds ["truncate table cached_prices; truncate table users cascade"]))

(defmethod db/reify-storage ::db/sql
  [config]
  (let [ds (jdbc/get-datasource config)]
    (reify db/Storage
      (put [_ models] (put* ds models))
      (select [_ criteria options] (select* ds criteria options))
      (delete [_ models] (delete* ds models))
      (close [_]) ; this is a no-op for next-jdbc
      (reset [_] (reset* ds)))))
