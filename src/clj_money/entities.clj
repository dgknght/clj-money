(ns clj-money.entities
  (:refer-clojure :exclude [find count update])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :as a]
            [clojure.core :as c]
            [clojure.data :refer [diff]]
            [clojure.walk :refer [postwalk]]
            [clojure.tools.logging :as log]
            [integrant.core :as ig]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util :refer [entity=]]
            [clj-money.db :as db]))

(s/def ::data-entity (s/and map?
                       util/entity-type))
(s/def ::puttable (s/or :map       ::data-entity
                        :operation (s/tuple ::db/operation ::data-entity)))
(s/def ::puttables (s/coll-of ::puttable :min-count 1))

(def exchanges #{:nyse :nasdaq :amex :otc})

(defprotocol CompositeID
  (components [this] "Returns the components of the ID"))

(def composite-id? (partial satisfies? CompositeID))

(s/def ::id (some-fn string?
                     uuid?
                     integer?
                     composite-id?))
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

(defn- after-read*
  [entity opts]
  (let [result (after-read entity opts)]
    (with-meta result {::original result})))

(defn- validation-key
  [m]
  (keyword "clj-money.entities"
           (-> m util/entity-type name)))

(defn validate
  ([entity] (validate entity {}))
  ([entity {:as opts :keys [suspend-validation]}]
   (when-not (contains? suspend-validation (util/entity-type entity))
     (when-let [data (s/explain-data (validation-key entity) entity)]
       (log/debugf "[validation] Invalid entity %s" (with-out-str (pprint data)))
       (throw (ex-info "Validation failed"
                       {::v/errors (v/extract-errors data opts)}))))
   entity))

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
              #(after-read* % options))
        (db/select (db/storage)
                   (prepare-criteria criteria)
                   options))))

(defn count
  ([criteria] (count criteria {}))
  ([criteria opts]
   (db/select (db/storage)
              criteria
              (assoc opts :count true))))

(defn find-by
  ([criteria] (find-by criteria {}))
  ([criteria options]
   (first (select criteria (assoc options :limit 1)))))

(defn- db-find
  [id]
  (db/find (db/storage) id))

(defn find
  "Return the entity having the specified ID"
  [id-or-entity & [opts]]
  (some-> id-or-entity
          util/->id
          db/unserialize-id
          db-find
          (after-read* opts)))

(defn find-many
  "Returns the entities having the specified IDs"
  [ids-or-entities & [opts]]
  (->> ids-or-entities
       util/->id
       db/unserialize-id
       (db/find-many (db/storage))
       (map #(after-read* % opts))))

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

(def ^:dynamic *publisher*
  "The ::publisher component that put-many and delete-many publish changes
  to. It's bound per request (see clj-money.web.handler) and per test. When
  it's unbound, nothing is published."
  nil)

(defn- topic
  "Returns the topic of a published message: the entity type of a change
  tuple, or the ::topic of a signal (see signal)."
  [msg]
  (if (map? msg)
    (::topic msg)
    (->> msg
         (filter identity)
         (some util/entity-type))))

(defmethod ig/init-key ::publisher
  ; Publishes [before after] change tuples, with the entity type as the
  ; topic. Subscribe with sub. The source channel is buffered so that
  ; publishing doesn't wait on slow subscribers.
  [_ {:keys [buffer-size] :or {buffer-size 1000}}]
  (let [ch (a/chan buffer-size)]
    {:ch ch
     :mult (a/mult ch)}))

(defmethod ig/halt-key! ::publisher
  ; Closing the source channel closes every subscribed channel.
  [_ {:keys [ch]}]
  (a/close! ch))

(defn sub
  "Returns a channel that receives the changes to entities of the given
  types (and the signals with the given topics, see signal) from publisher,
  in the order in which they were published, even across topics.

  Unlike core.async/pub, which delivers each topic separately, this taps a
  single mult, so that a signal can't overtake the changes published before
  it."
  ([publisher topics] (sub publisher topics {}))
  ([publisher topics {:keys [buf-or-n] :or {buf-or-n 1000}}]
   (let [topics (set topics)
         ch (a/chan buf-or-n (filter (comp topics topic)))]
     (a/tap (:mult publisher) ch)
     ch)))

(defn unsub
  "Stops delivery to (and closes) a channel returned by sub."
  [publisher ch]
  (a/untap (:mult publisher) ch)
  (a/close! ch))

(defn- publish*
  [{:keys [ch]} msgs]
  ; put! (not onto-chan!) so that messages keep the order in which they were
  ; published, even across calls, and so that publishing never blocks,
  ; even when called from inside a go block. The ::published-at metadata
  ; (from System/nanoTime) lets subscribers measure how long a message
  ; waited to be handled.
  (doseq [msg msgs]
    (a/put! ch (vary-meta msg assoc ::published-at (System/nanoTime)))))

(defn signal
  "Publishes msg (a map) with the given topic, after any changes already
  published. Subscribers use signals to learn when they have handled
  everything published before it (see clj-money.entities.propagation/await-propagation)."
  [publisher topic msg]
  (publish* publisher [(assoc msg ::topic topic)]))

(defn- publish
  "Publishes the given change tuples to the publisher in opts (or
  *publisher*), if there is one."
  [opts changes]
  (when-let [publisher (if (contains? opts :publisher)
                         (:publisher opts)
                         *publisher*)]
    (publish* publisher changes)))

(defn put-many
  "Save a sequence of entities to the database, providing lifecycle hooks that
  are dispatched by entity type, including: before-validation, before-save,
  after-save, etc.

  Change tuples containing the before and after versions of each entity
  affected by the operation are published to the publisher (see
  *publisher*), with the entity type as the topic.

  Options:
  :on-duplicate - one of :merge-last-wins, :merge-first-wins, or :throw
  :publisher    - The ::publisher component to publish changes to, in place
                  of *publisher*. Pass nil to publish nothing."
  ([entities] (put-many {} entities))
  ([{:as opts
     :keys [storage]}
    entities]
   {:pre [(s/valid? ::puttables entities)]}

   (let [to-save (->> entities
                      (handle-dupes opts)
                      (map (dispatch
                             (comp ensure-id
                                   before-save
                                   #(validate % opts)
                                   before-validation))))
         result (->> to-save
                     (db/put (or storage (db/storage)) opts)
                     (map (comp append-before
                                after-save
                                #(after-read* % {}))))]
     (publish opts (calc-changes to-save result))
     result)))

(defn put
  [entity & {:as opts}]
  (first (put-many opts [entity])))

(defn update
  [changes criteria]
  (db/update (db/storage) changes criteria))

(defn delete-many
  "Removes the entities from the database and publishes a change tuple for
  each (see put-many). Accepts the :publisher option, as put-many does."
  ([entities] (delete-many {} entities))
  ([opts entities]
   {:pre [(seq (filter identity entities))]}
   (let [result (->> entities
                     (map before-delete)
                     (db/delete (db/storage)))]
     (publish opts (map #(vector % nil) entities))
     result)))

(defn delete
  [entity & {:as opts}]
  {:pre [entity]}
  (delete-many opts [entity]))

(defn purge!
  [entity]
  {:pre [entity]}
  (db/purge! (db/storage) entity))

(defn resolve-ref
  [entity-or-ref]
  {:pre [(map? entity-or-ref)]}
  (if (util/entity-ref? entity-or-ref)
    (find (:id entity-or-ref))
    entity-or-ref))

(def sensitive-keys
  #{:user/email
    :user/password
    :user/password-reset-token
    :user/token-expires-at
    :identity/provider-id
    :invitation/token
    :oauth2/access-tokens})

(defn scrub-sensitive-data
  [m]
  (postwalk (fn [x]
              (if (and (map-entry? x)
                       (sensitive-keys (key x)))
                (assoc-in x [1] "********")
                x))
            m))
