(ns clj-money.models
  (:refer-clojure :exclude [find count update])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.validation :as v]
            [dgknght.app-lib.models :refer [->id]]
            [clj-money.models :as models]
            [clj-money.json] ; to ensure encoders are registered
            [clj-money.util :as util :refer [model=]]
            [clj-money.db :as db]))

(def exchanges #{:nyse :nasdaq :amex :otc})

(s/def ::id (some-fn uuid? int? util/temp-id?))
(s/def ::model-ref (s/keys :req-un [::id]))

(defmulti prepare-criteria util/model-type-dispatch)
(defmethod prepare-criteria :default [m] m)

(defmulti before-validation util/model-type-dispatch)
(defmethod before-validation :default [m & _] m)

(defmulti propagate util/model-type-dispatch)
(defmethod propagate :default [m] [m])

(defmulti before-save util/model-type-dispatch)
(defmethod before-save :default [m & _] m)

(defmulti after-save util/model-type-dispatch)
(defmethod after-save :default [m & _] m)

(defmulti after-read util/model-type-dispatch)
(defmethod after-read :default [m & _] m)

(defmulti before-delete util/model-type-dispatch)
(defmethod before-delete :default [m & _] m)

(defmulti propagate-delete util/model-type-dispatch)
(defmethod propagate-delete :default [m] [m])

(defn- validation-key
  [m]
  (keyword "clj-money.models"
           (-> m util/model-type name)))

(defn- validate
  [model]
  (let [validated (v/validate model (validation-key model))]
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
  (select (util/model-type {:id [:in (mapv ->id m-or-ids)]}
                         model-type)))

(defn find
  "Find a model by id or by reference map.

  When given one argument:
    - If the argument is a model reference, return the model
    - If the argument is a keyword, a function that will look up models of the specified type
  When given two arguments, look up a model of the spcified type having the specified id"
  ([arg]
   (if (keyword? arg)
     #(find % arg)
     (do
       (assert (:id arg) "The argument must have an id")
       (assert (util/model-type arg) "The argument must have a model type")
       (find (:id arg)
           (keyword (util/model-type arg)))))) ; TODO: can we remove the call to keyword?
  ([id-or-ref model-type]
   {:pre [id-or-ref (keyword? model-type)]}
   (find-by (util/model-type (util/->model-ref id-or-ref)
                           model-type))))

(defn- merge-dupes
  "Given a sequence of models, merge any that have the same id"
  [models]
  (loop [input models output []]
    (if-let [model (first input)]
      (if (:id model)
        (let [{dupes true
               others false} (group-by #(model= model %)
                                       (rest input))]
          (recur others (conj output (apply merge model dupes))))
        (recur (rest input) (conj output model)))
      output)))

(defn- dispatch*
  [f x]
  (if (vector? x)
    (let [[oper :as puttable] x]
      (if (= ::db/delete oper)
        puttable
        (update-in puttable [1] f)))
    (f x)))

(defn- dispatch
  "Returns a function that accepts either a naked model or a model wrapped in a
  vector with a db operator in the first position, and applies the function to
  the model, unless it's a delete operation."
  [f]
  (partial dispatch* f))

(defn- dispatch-propagation
  [x]
  (if (vector? x)
    (let [[oper m] x
          f (if (= ::db/delete oper)
              propagate-delete
              propagate)
          [r & rs] (f m)]
      (cons [oper r]
            rs))
    (propagate x)))

(defn put-many
  [models]
  (->> models
       (map (dispatch
              (comp validate
                    before-validation)))
       (mapcat dispatch-propagation)
       (map (dispatch before-save))
       (merge-dupes)
       (db/put (db/storage))
       (map (comp append-before
                  after-save
                  #(after-read % {}))))) ; The empty hash here is for options

(defn put
  [model]
  (first (put-many [model])))

(defn delete-many
  [models]
  {:pre [(seq (filter identity models))]}
  (->> models
       (map before-delete)
       (mapcat propagate-delete)
       (db/delete (db/storage))))

(defn delete
  [model]
  {:pre [model]}
  (delete-many [model]))

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
