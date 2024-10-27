(ns clj-money.models
  (:refer-clojure :exclude [find count])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.validation :as v]
            [dgknght.app-lib.models :refer [->id]]
            [clj-money.util :as util]
            [clj-money.db :as db]))

(s/def ::exchange #{:nyse :nasdaq :amex :otc})
(s/def ::model-ref (s/and map? #(contains? % :id)))

(defmulti prepare-criteria db/type-dispatch)
(defmethod prepare-criteria :default [m] m)

(defmulti before-validation db/type-dispatch)
(defmethod before-validation :default [m & _] m)

(defmulti propagate db/type-dispatch)
(defmethod propagate :default [m] [m])

(defmulti before-save db/type-dispatch)
(defmethod before-save :default [m & _] m)

(defmulti after-save db/type-dispatch)
(defmethod after-save :default [m & _] m)

(defmulti after-read db/type-dispatch)
(defmethod after-read :default [m & _] m)

(defmulti propagate-delete db/type-dispatch)
(defmethod propagate-delete :default [m & _] m)

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

(defn find-many
  [m-or-ids model-type]
  (select (db/model-type {:id [:in (mapv ->id m-or-ids)]}
                         model-type)))

(defn find
  ([{:keys [id] :as m}]
   {:pre [(map? m) (:id m)]}
   (find id (keyword (util/qualifier m))))
  ([id-or-ref model-type]
   {:pre [id-or-ref (keyword? model-type)]}
   (find-by (db/model-type (util/->model-ref id-or-ref)
                           model-type))))

(defn realize
  [model-or-ref model-type]
  {:pre [(map? model-or-ref)]}
  (if (= 1 (count model-or-ref))
    (find model-or-ref model-type)
    model-or-ref))

(defn put-many
  [& models]
  (->> models
       (map (comp validate
                  before-validation))
       (mapcat propagate)
       (map (comp before-save
                  validate ; TODO: Should it be possible for propagation to produce invalid models?
                  before-validation))
       (db/put (db/storage))
       (map (comp after-save
                  #(after-read % {})))))

(defn put
  [model]
  (first (put-many model)))

(defn delete-many
  [& models]
  (->> models
       (mapcat (comp (fn [[m & ms]]
                       (cons [::db/delete m]
                             ms))
                  propagate-delete))
       (db/put (db/storage))))

(defn delete
  [model]
  (delete-many model))
