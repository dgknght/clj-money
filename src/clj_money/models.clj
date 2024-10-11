(ns clj-money.models
  (:refer-clojure :exclude [find count])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [clj-money.util :as util]
            [clj-money.db :as db]))

(s/def ::exchange #{:nyse :nasdaq :amex :otc})

(defmulti validate db/model-type)

(defn select
  ([criteria] (select criteria {}))
  ([criteria options]
   (db/select (db/storage) criteria options)))

(defn count
  [criteria]
  (:record-count (select criteria {:count true})))

(defn find-by
  ([criteria] (find-by criteria {}))
  ([criteria options]
   (first (select criteria (assoc options :limit 1)))))

(defn find
  ([{:keys [id] :as m}]
   {:pre [(map? m) (:id m)]}
   (find id (keyword (util/qualifier m))))
  ([id model-type]
   {:pre [id (keyword? model-type)]}
   (find-by (db/model-type {:id id} model-type))))

(defn put-many
  [& models]
  (->> models
       (map validate)
       (db/put (db/storage))))

(defn put
  [model]
  (first (put-many model)))

(defn delete
  [model]
  (db/delete (db/storage) model))
