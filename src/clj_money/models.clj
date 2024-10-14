(ns clj-money.models
  (:refer-clojure :exclude [find count])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util]
            [clj-money.db :as db]))

(s/def ::exchange #{:nyse :nasdaq :amex :otc})
(s/def ::model-ref (s/and map? #(contains? % :id)))

(defmulti prepare-criteria db/model-type-dispatch)
(defmethod prepare-criteria :default [m] m)

(defmulti deconstruct db/model-type-dispatch)
(defmethod deconstruct :default [m] [m])

(defmulti before-validation db/model-type-dispatch)
(defmethod before-validation :default [m & _] m)

(defmulti before-save db/model-type-dispatch)
(defmethod before-save :default [m & _] m)

(defmulti after-save db/model-type-dispatch)
(defmethod after-save :default [m & _] m)

(defmulti after-read db/model-type-dispatch)
(defmethod after-read :default [m & _] m)

(defn- validate
  [model]
  (let [validated (v/validate model (keyword "clj-money.models"
                                             (name (db/model-type model))))]
    (when (seq (::v/errors validated))
      (throw (ex-info "Validation failed" (select-keys validated [::v/errors])))))
  model)

(defn select
  ([criteria] (select criteria {}))
  ([criteria options]
   (map #(after-read % options)
        (db/select (db/storage)
                   (prepare-criteria criteria)
                   options))))

(defn count
  [criteria]
  (:record-count (db/select (db/storage) criteria {:count true})))

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
       (map (comp before-save
                  validate
                  before-validation))
       (mapcat deconstruct)
       (db/put (db/storage))
       (map (comp after-save
                  #(after-read % {})))))

(defn put
  [model]
  (first (put-many model)))

(defn delete-many
  [& models]
  (db/delete (db/storage) models))

(defn delete
  [model]
  (delete-many model))
