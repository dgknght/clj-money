(ns clj-money.models
  (:refer-clojure :exclude [find count update])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.validation :as v]
            [dgknght.app-lib.models :refer [->id]]
            [clj-money.json] ; to ensure encoders are registered
            [clj-money.util :as util]
            [clj-money.db :as db]))

(def exchanges #{:nyse :nasdaq :amex :otc})

(s/def ::id (some-fn uuid? int? util/temp-id?))
(s/def ::model-ref (s/keys :req-un [::id]))

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

(defmulti before-delete db/type-dispatch)
(defmethod before-delete :default [m & _] m)

(defmulti propagate-delete db/type-dispatch)
(defmethod propagate-delete :default [m & _] [m])

(defn- validate
  [model]
  (let [validated (v/validate model (keyword "clj-money.models"
                                             (name (db/model-type model))))]
    (when (seq (::v/errors validated))
      (throw (ex-info "Validation failed" (select-keys validated [::v/errors])))))
  model)

(defn before
  [model k]
  (-> model
      meta
      ::before
      k))

(defn- append-before
  [model]
  (vary-meta model assoc ::before model))

(defn select
  ([criteria] (select criteria {}))
  ([criteria options]
   (map (comp append-before
              #(after-read % options))
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
   (find id (keyword (db/model-type m))))
  ([id-or-ref model-type]
   {:pre [id-or-ref (keyword? model-type)]}
   (find-by (db/model-type (util/->model-ref id-or-ref)
                           model-type))))

(defn- merge-dupes
  "Given a sequence of models, merge any that have the same id"
  [models]
  (loop [input models output []]
    (if-let [model (first input)]
      (if-let [id (:id model)]
        (let [{dupes true
               others false} (group-by #(= id (:id %)) (rest input))]
          (recur others (conj output (apply merge model dupes))))
        (recur (rest input) (conj output model)))
      output)))

(defn put-many
  [models]
  (->> models
       (map (comp validate
                  before-validation))
       (mapcat propagate)
       (map before-save)
       (merge-dupes)
       (db/put (db/storage))
       (map (comp append-before
                  after-save
                  #(after-read % {})))))

(defn put
  [model]
  (first (put-many [model])))

(defn delete-many
  [models]
  (->> models
       (map before-delete)
       (mapcat (comp (fn [[m & ms]]
                       (cons [::db/delete m]
                             ms))
                     propagate-delete))
       (db/put (db/storage))))

(defn delete
  [model]
  (delete-many model))

(defn update
  "Updates multiple records in the data store against criteria instead of a
  single model or group of models"
  [criteria changes]
  (db/update (db/storage) criteria changes))

(defn resolve-ref
  ([model-type]
   (fn [model-or-ref]
     (resolve-ref model-or-ref model-type)))
  ([model-or-ref model-type]
   (if (util/model-ref? model-or-ref)
     (find model-or-ref model-type)
     model-or-ref)))
