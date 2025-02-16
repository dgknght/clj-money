(ns clj-money.models
  (:refer-clojure :exclude [find count update])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :as a]
            [clojure.set :refer [map-invert]]
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

(defmulti propagate (fn [b a]
                      (util/model-type (or b a))))
(defmethod propagate :default [_before _after] [])

(defmulti before-save util/model-type-dispatch)
(defmethod before-save :default [m & _] m)

(defmulti after-save util/model-type-dispatch)
(defmethod after-save :default [m & _] m)

(defmulti after-read util/model-type-dispatch)
(defmethod after-read :default [m & _] m)

(defmulti before-delete util/model-type-dispatch)
(defmethod before-delete :default [m & _] m)

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
  ([model]
   (-> model
       meta
       ::before))
  ([model k]
   (-> model
       before
       k)))

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

(def ^:private mergeable?
  (every-pred map? :id))

(defn- merge-dupes
  "Given a sequence of models, merge any that have the same id"
  [puttables]
  (loop [input puttables output []]
    (if-let [puttable (first input)]
      (if (mergeable? puttable)
        (let [{dupes true
               others false} (group-by #(model= puttable %)
                                       (rest input))]
          (recur others (conj output (apply merge puttable dupes))))
        (recur (rest input) (conj output puttable)))
      output)))

(defn- duplicate-present?
  [ms]
  (->> ms
       (remove vector?)
       (map (juxt util/model-type :id))
       (frequencies)
       (remove (comp #{1} second))
       seq))

(defn- throw-on-duplicate
  [ms]
  (when (duplicate-present? ms)
    (throw (ex-info "Duplicate model found" {:models ms})))
  ms)

(defn- handle-dupes
  [{:keys [on-duplicate] :or {on-duplicate :throw}} ms]
  (case on-duplicate
    :merge-last-wins (merge-dupes ms)
    :throw (throw-on-duplicate ms)
    (throw (ex-info "Invalid on-duplicate value" {:on-duplicate on-duplicate}))))


(defn- dispatch
  "Returns a function that accepts either a naked model or a model wrapped in a
  vector with a db operator in the first position, and applies the function to
  the model, unless it's a delete operation."
  ([f]
   #(dispatch f %))
  ([f x]
   (if (vector? x)
     (let [[oper :as puttable] x]
       (if (= ::db/delete oper)
         puttable
         (update-in puttable [1] f)))
     (f x))))

(defn- ensure-id
  "When saving a new record, make sure we have a temp id"
  [m]
  (if (map? m) ; this could also be a vector like [:delete {:id 123}]
    (update-in m [:id] (fn [id]
                         (or id (util/temp-id))))
    m))

(defn- deletions
  [ms]
  (filter (every-pred vector?
                      #(= ::db/delete (first %)))
          ms))

(defn put-many
  "Save a sequence of models to the database, providing lifecycle hooks that
  are dispatched by model type, including: before-validation, before-save,
  after-save.

  Additionally, propagates changes to related records, also dispatched by model
  type.

  Options:
  :on-duplicate - one of :merge-last-wins, :merge-first-wins, or :throw
  :prop-chan    - An async channel, that when passed, receives the result of
                  the propagation. When not passed, the propagation is done
                  synchronously and the result is included with the primary result."
  ([models] (put-many {} models))
  ([{:as opts
     :keys [prop-chan
            depth
            storage]
     :or {depth 0}}
    models]
   {:pre [(s/valid? (s/coll-of map?) models)]}

   (if (> depth 3)
     (throw (ex-info "Excessive recursion" {:depth depth}))
     (let [to-save (->> models
                        (handle-dupes opts)
                        (map (dispatch
                               (comp ensure-id
                                     before-save
                                     validate
                                     before-validation))))
           before-map (->> to-save
                           (map (juxt :id (dispatch before)))
                           (into {}))
           {:keys [saved id-map]} (db/put (or storage
                                              (db/storage))
                                          to-save)
           primary-result (map (comp append-before
                                     after-save
                                     #(after-read % {}))
                               saved)
           inverted-id-map (map-invert id-map)
           propagation-to-save (->> (deletions models)
                                    (concat primary-result)
                                    (map (fn [{:keys [id] :as a}]
                                           [(before-map (or (inverted-id-map id)
                                                            id))
                                            a]))
                                    (mapcat #(apply propagate %))
                                    seq)]
       (if prop-chan
         (do
           (when propagation-to-save
             (a/go
               (a/onto-chan! prop-chan
                             (put-many {:depth (inc depth)} propagation-to-save))))
           primary-result)
         (concat primary-result
                 (when propagation-to-save
                   (put-many {:depth (inc depth)} propagation-to-save))))))))

(defn put
  [model & {:as opts}]
  (first (put-many opts [model])))

(defn delete-many
  ([models] (delete-many {} models))
  ([{:keys [prop-chan]} models]
   {:pre [(seq (filter identity models))]}
   (let [propagation (mapcat #(propagate % nil)
                             models)]
     (db/delete (db/storage) models)
     (when propagation
       (if prop-chan
         (a/go
           (a/onto-chan! prop-chan
                         (put-many {} propagation)))
         (put-many {} propagation))))))

(defn delete
  [model & {:as opts}]
  {:pre [model]}
  (delete-many opts [model]))

(defn resolve-ref
  ([model-type]
   (fn [model-or-ref]
     (resolve-ref model-or-ref model-type)))
  ([model-or-ref model-type]
   (if (util/model-ref? model-or-ref)
     (find model-or-ref model-type)
     model-or-ref)))
