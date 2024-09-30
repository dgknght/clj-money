(ns clj-money.db.sql
  (:require [next.jdbc :as jdbc]
            [clj-money.db :as db]
            [clj-money.db.sql.types :refer [temp-id?]]))

(defn- put-one
  [_ds [_action _model]])

(defmulti before-save db/type-dispatch)

(defmethod before-save :default [m] m)

(defn- wrap-oper
  "Ensure that what we are passing on is a tuple with a database
  operation in the 1st position and a model in the second."
  [m]
  (cond
    (vector? m) m
    (:id m)     [:update m]
    :else       [:insert m]))

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
  [db {:as result :keys [id-map]} [operator m]]
  (let [id-resolved (cond-> m
                      (seq id-map) (resolve-temp-ids id-map)
                      (temp-id? m) (dissoc :id))
        saved (put-one db [operator id-resolved])]
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

(defn- select* [_ds _criteria _options])
(defn- delete* [_ds _models])

(defn- reset*
  [ds]
  (jdbc/execute! ds ["truncate table users cascade"]))

(defmethod db/reify-storage ::db/sql
  [config]
  (let [ds (jdbc/get-datasource config)]
    (reify db/Storage
      (put [_ models] (put* ds models))
      (select [_ criteria options] (select* ds criteria options))
      (delete [_ models] (delete* ds models))
      (close [_]) ; this is a no-op for next-jdbc
      (reset [_] (reset* ds)))))
