(ns clj-money.entities.propagation
  "Propagates changes to entities to the entities that depend on them, e.g.,
  account balances when a transaction changes.

  Changes are published by clj-money.entities/put-many and delete-many (see
  clj-money.entities/*publisher*). The ::subscription component subscribes
  to them and propagates each one in the background, in the order in which
  they were published. Model namespaces implement the propagate multimethod
  for the entity types whose changes need to be propagated."
  (:require [clojure.core.async :as a]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [integrant.core :as ig]
            [clj-money.util :as util]
            [clj-money.db :as db]
            [clj-money.otel :as otel]
            [clj-money.entities :as entities])
  (:import clojure.core.async.impl.channels.ManyToManyChannel))

; A map of priority number to sets of propagation fns
(def ^:private full-propagations (atom {}))

(defn- propagation-fn [opts]
  (->> @full-propagations
       (sort-by first >)
       (mapcat second)
       (map (fn [f] #(f % opts)))
       (apply comp)))

(defn propagate-all
  "Performs a full propagation for all entities in the system.

  Model namespaces register a function with add-full-propagation so that when
  this function is called, all of the mode-specific propagations will be executed."
  ([]
   (propagate-all {}))
  ([opts]
   (doseq [entity (entities/select (util/entity-type {} :entity))]
     (propagate-all entity opts)))
  ([entity opts]
   (log/debugf "[propagation] start entity %s"
               (:entity/name entity))
   (let [f (propagation-fn opts)]
     (try
       (f entity)
       (log/infof "[propagation] finished entity %s"
                  (:entity/name entity))
       (catch Exception e
         (log/error e "[propagation] Unable to finish the full propagation"))))))

(defn add-full-propagation
  "Registers a function that will be executed with propagate-all is invoked.

  The function must take an entity as the first argument, and options
  as the second argument. The function must return the entity."
  [f & {:keys [priority] :or {priority 10}}]
  (swap! full-propagations update-in [priority] (fnil conj #{}) f))

(defmulti propagate
  "Given a [before after] change tuple, returns the entities (or
  operations, like [::db/delete entity]) to be put as a result of the
  change."
  #(->> %
        (filter identity)
        (some util/entity-type)))
(defmethod propagate :default [[_before _after]] [])

(defn- propagate-change
  [change]
  (when-let [updates (seq (propagate change))]
    ; The updates are not published in turn: a propagation computes
    ; everything affected by the original change, so propagating the
    ; updates would only repeat (or, for partial entities, corrupt) work.
    (entities/put-many {:publisher nil} updates)))

(defn- topics
  "Returns the entity types that have a propagate implementation."
  []
  (disj (set (keys (methods propagate))) :default))

(def ^:private duration
  (otel/histogram "clj_money.propagation.duration"
                  :description "Time spent propagating a change. Failed propagations have an error.type attribute"
                  :unit "s"
                  :buckets otel/duration-buckets))

(def ^:private lag
  (otel/histogram "clj_money.propagation.lag"
                  :description "Time from when a change is published until it has been propagated"
                  :unit "s"
                  :buckets otel/duration-buckets))

(defn- seconds-since
  [nanos]
  (/ (- (System/nanoTime) nanos) 1e9))

(defn- handle
  [msg]
  (if-let [ack (::ack msg)]
    (a/put! ack true)
    (let [entity-type (util/entity-type (some identity msg))
          attributes {"entity.type" entity-type}
          started (System/nanoTime)
          record (fn [attributes]
                   (otel/record duration (seconds-since started) attributes)
                   (when-let [published-at (::entities/published-at (meta msg))]
                     (otel/record lag (seconds-since published-at) attributes)))]
      (try
        (propagate-change msg)
        (record attributes)
        (log/debugf "[propagation] propagated %s" entity-type)
        ; Throwable, not Exception, so that a failed assertion (e.g. a :pre
        ; condition) doesn't end the subscription's thread, after which no
        ; other change would be propagated
        (catch Throwable e
          (record (assoc attributes "error.type" (.getName (class e))))
          (log/errorf e "[propagation] unable to propagate change %s" (pr-str msg)))))))

(defn- queue-size
  "Returns the number of messages waiting in the channel: those in its
  buffer plus pending puts. core.async throws once a channel has 1024
  pending puts, so the publisher's queue must stay well below its buffer
  size plus 1024."
  [^ManyToManyChannel ch]
  (+ (count (.buf ch))
     (.size (.puts ch))))

(defn- observe-queue-sizes
  [publisher ch]
  (otel/gauge "clj_money.propagation.queue.size"
              (fn []
                [[(queue-size (:ch publisher)) {"queue" "publisher"}]
                 [(queue-size ch) {"queue" "subscription"}]])
              :description "Number of published messages waiting to be propagated"
              :unit "{message}"))

(defmethod ig/init-key ::subscription
  ; Subscribes to changes from :publisher for every entity type that has a
  ; propagate implementation and propagates them one at a time, on a
  ; background thread, with :storage bound. Propagating one at a time keeps
  ; changes to the same account from racing one another.
  [_ {:keys [publisher storage]}]
  (let [id (keyword (name ::flush) (str (random-uuid)))
        ch (entities/sub publisher (conj (topics) id))]
    {:ch ch
     :id id
     :publisher publisher
     :queue-sizes (observe-queue-sizes publisher ch)
     :done (a/thread
             (binding [db/*storage* (or storage db/*storage*)
                       entities/*publisher* nil]
               (loop []
                 (when-let [msg (a/<!! ch)]
                   (handle msg)
                   (recur)))))}))

(defmethod ig/halt-key! ::subscription
  ; Changes received before halting are still propagated.
  [_ {:keys [ch publisher done queue-sizes]}]
  (otel/close-gauge queue-sizes)
  (entities/unsub publisher ch)
  (a/<!! done))

(defn await-propagation
  "Blocks until the subscription has propagated every change that was
  published before the call, or until the timeout (in milliseconds,
  default 10 seconds) elapses. Returns true if propagation finished, false
  if it timed out."
  ([subscription] (await-propagation subscription {}))
  ([{:keys [publisher id]} {:keys [timeout-ms] :or {timeout-ms 10000}}]
   (let [ack (a/promise-chan)]
     (entities/signal publisher id {::ack ack})
     (boolean (first (a/alts!! [ack (a/timeout timeout-ms)]))))))
