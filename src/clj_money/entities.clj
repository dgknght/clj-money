(ns clj-money.entities
  (:refer-clojure :exclude [find count update])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :as a]
            [clojure.core :as c]
            [clojure.data :refer [diff]]
            [clojure.walk :refer [postwalk]]
            [clojure.tools.logging :as log]
            [dgknght.app-lib.validation :as v]
            [dgknght.app-lib.models :refer [->id]]
            [clj-money.json] ; to ensure encoders are registered
            [clj-money.util :as util :refer [entity=]]
            [clj-money.db :as db]))

(s/def ::data-entity (s/and map?
                       util/entity-type))
(s/def ::puttable (s/or :map       ::data-entity
                        :operation (s/tuple ::db/operation ::data-entity)))
(s/def ::puttables (s/coll-of ::puttable :min-count 1))

(def exchanges #{:nyse :nasdaq :amex :otc})

(s/def ::id (some-fn uuid? int? util/temp-id?))
(s/def ::entity-ref (s/keys :req-un [::id]))

(defmulti prepare-criteria util/entity-type-dispatch)
(defmethod prepare-criteria :default [m] m)

(defmulti before-validation util/entity-type-dispatch)
(defmethod before-validation :default [m & _] m)

(defmulti before-save util/entity-type-dispatch)
(defmethod before-save :default [m & _] m)

(defmulti after-save util/entity-type-dispatch)
(defmethod after-save :default [m & _] m)

(defmulti after-read util/entity-type-dispatch)
(defmethod after-read :default [m & _] m)

(defmulti before-delete util/entity-type-dispatch)
(defmethod before-delete :default [m & _] m)

(defn- validation-key
  [m]
  (keyword "clj-money.entities"
           (-> m util/entity-type name)))

(defn validate
  [entity]
  (let [validated (v/validate entity (validation-key entity))]
    (when-let [errors (seq (::v/errors validated))]
      (log/debugf "[validation] Invalid entity %s: %s"
                  entity
                  errors)
      (throw (ex-info "Validation failed" (select-keys validated [::v/errors])))))
  entity)

(defn before
  ([entity]
   (-> entity
       meta
       ::before))
  ([entity k]
   (-> entity
       before
       k)))

(defn- append-before
  [entity]
  (vary-meta entity assoc ::before entity))

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
  (db/select (db/storage) criteria {:count true}))

(defn find-by
  ([criteria] (find-by criteria {}))
  ([criteria options]
   (first (select criteria (assoc options :limit 1)))))

(defn find-many
  [m-or-ids entity-type]
  (select (util/entity-type {:id [:in (mapv ->id m-or-ids)]}
                           entity-type)))

(defn find
  "Find a entity by id or by reference map.

  When given one argument:
    - If the argument is a entity reference, return the entity
    - If the argument is a keyword, a function that will look up entities of the specified type
  When given two arguments, look up a entity of the spcified type having the specified id"
  ([arg]
   (if (keyword? arg)
     #(find % arg)
     (do
       (assert (:id arg) "The argument must have an id")
       (assert (util/entity-type arg) "The argument must have a entity type")
       (find (:id arg)
             (keyword (util/entity-type arg)))))) ; TODO: can we remove the call to keyword?
  ([id-or-ref entity-type-or-opts]
   (let [[entity-type opts] (if (keyword? entity-type-or-opts)
                             [entity-type-or-opts {}]
                             [(or (:entity-type entity-type-or-opts)
                                  (util/entity-type id-or-ref))
                              entity-type-or-opts])]
     (find-by (util/entity-type
                (util/->entity-ref id-or-ref)
                entity-type)
              opts))))

(def ^:private mergeable?
  (every-pred map? :id))

(defn- merge-dupes
  "Given a sequence of entities, merge any that have the same id"
  [puttables]
  (loop [input puttables output []]
    (if-let [puttable (first input)]
      (if (mergeable? puttable)
        (let [{dupes true
               others false} (group-by #(entity= puttable %)
                                       (rest input))]
          (recur others (conj output (apply merge puttable dupes))))
        (recur (rest input) (conj output puttable)))
      output)))

(defn- duplicate-present?
  [ms]
  (->> ms
       (remove vector?)
       (filter :id)
       (map (juxt util/entity-type :id))
       (frequencies)
       (remove (comp #{1} second))
       seq))

(defn- throw-on-duplicate
  [ms]
  (when (duplicate-present? ms)
    (throw (ex-info "Duplicate entity found" {:entities ms})))
  ms)

(defn- handle-dupes
  [{:keys [on-duplicate] :or {on-duplicate :throw}} ms]
  (case on-duplicate
    :merge-last-wins (merge-dupes ms)
    :throw (throw-on-duplicate ms)
    (throw (ex-info "Invalid on-duplicate value" {:on-duplicate on-duplicate}))))

(defn- dispatch
  "Returns a function that accepts either a naked entity or a entity wrapped in a
  vector with a db operator in the first position, and applies the function to
  the entity, unless it's a delete operation."
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

(def ^:private deletion?
  (every-pred vector?
              #(= ::db/delete (first %))))

(defn- changed?
  [[before after]]
  (let [[missing extra] (diff before after)]
    (or (seq missing) (seq extra))))

^{:clj-kondo/ignore true}
(defn- change
  [[before after]]
  (cond
    (and before after)
    (let [[b a] (diff before after)]
      [:changed b a])

    before [:deleted (util/simplify before)]

    after [:inserted (util/simplify after)]))

(defn- calc-changes
  "Given a sequence of entities that are to be saved and the corresponding sequence
  of saved entities, calculate the difference tuples"
  [to-save saved]
  (->> saved
       (interleave to-save)
       (partition 2)
       (map (fn [[input after]]
              (if (deletion? input)
                [(second input) nil]
                [(before input) after])))
       (filterv changed?)))

(defn- emit-changes
  [{:keys [to-save
           saved
           out-chan
           close-chan?
           ctrl-chan]
    :or {close-chan? true}}]
  (when out-chan
    (a/go
      (when-let [changes (seq (calc-changes to-save saved))]
        (when ctrl-chan
          (a/>! ctrl-chan :start))
        (let [c (a/onto-chan! out-chan changes close-chan?)]
          (when ctrl-chan
            (a/<!! c)
            (a/>! ctrl-chan :finish)))))))

(defn put-many
  "Save a sequence of entities to the database, providing lifecycle hooks that
  are dispatched by entity type, including: before-validation, before-save,
  after-save, etc.

  Options:
  :on-duplicate - one of :merge-last-wins, :merge-first-wins, or :throw
  :out-chan     - An async channel that when passed, receives change tuples
                  containing before and after versions of each entity affected
                  by the operation.
  :close-chan?  - A boolean value indicating whether or not the out-chan should be
                  closed automatically once all pending results have been sent.
  :ctrl-chan    - Gets a message (:start) when changes are about to be emitted
                  another (:finish) when the changes have been emitted"
  ([entities] (put-many {} entities))
  ([{:as opts
     :keys [storage]}
    entities]

   (when-not (s/valid? ::puttables entities)
     (s/explain ::puttables entities)
     (throw (ex-info "Invalid entity" {:entities entities})))


   #_{:pre [(s/valid? ::puttables entities)]}

   (let [to-save (->> entities
                      (handle-dupes opts)
                      (map (dispatch
                             (comp ensure-id
                                   before-save
                                   validate
                                   before-validation))))
         saved (db/put (or storage
                           (db/storage))
                       to-save)
         result (map (comp append-before
                           after-save
                           #(after-read % {}))
                     saved)]
     (emit-changes (assoc opts
                          :to-save to-save
                          :saved result))
     result)))

(defn put
  [entity & {:as opts}]
  (first (put-many opts [entity])))

(defn update
  [changes criteria]
  (db/update (db/storage) changes criteria))

(defn delete-many
  ([entities] (delete-many {} entities))
  ([{:keys [out-chan]} entities]
   {:pre [(seq (filter identity entities))]}
   (let [result (->> entities
                     (map before-delete)
                     (db/delete (db/storage)))]
     (when out-chan
       (a/go
         (->> entities
              (map #(vector % nil))
              (a/onto-chan!! out-chan))))
     result)))

(defn delete
  [entity & {:as opts}]
  {:pre [entity]}
  (delete-many opts [entity]))

(defn resolve-ref
  ([entity-type]
   (fn [entity-or-ref]
     (resolve-ref entity-or-ref entity-type)))
  ([entity-or-ref entity-type]
   (if (util/entity-ref? entity-or-ref)
     (find entity-or-ref entity-type)
     entity-or-ref)))

(def sensitive-keys
  #{:user/email
    :user/password
    :user/password-reset-token
    :identity/provider-id})

(defn scrub-sensitive-data
  [m]
  (postwalk (fn [x]
              (if (and (map-entry? x)
                       (sensitive-keys (key x)))
                (assoc-in x [1] "********")
                x))
            m))
