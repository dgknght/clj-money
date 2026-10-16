(ns clj-money.monitor-cache
  "Caches budget monitors (see clj-money.reports/monitor), which are slow
  to compute, for the dashboard.

  A monitor is cached by account and as-of date, so the cached monitors
  for one day aren't used on the next. Cached monitors are evicted:

  - when a change to a transaction has been propagated, for the monitors
    that report on an account of one of its items (before or after the
    change). Monitors are computed from propagated values, like the
    balances of the transaction items, so the eviction waits for the
    ::prop/propagated signal rather than the change itself.
  - when a budget, budget item or account changes, for every monitor, as
    these change rarely.
  - after :ttl-minutes (default 60), to cover changes that aren't
    published, like imports and work done in the REPL."
  (:require [clojure.core.async :as a]
            [clojure.set :refer [intersection]]
            [clojure.tools.logging :as log]
            [integrant.core :as ig]
            [java-time.api :as t]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.entities.propagation :as prop]
            [clj-money.reports :as rpt]))

(def ^:private eviction-topics
  #{:budget :budget-item :account ::prop/propagated})

(defn- now-millis []
  (System/currentTimeMillis))

(defn- evict
  "Removes the entries for which (pred entry) is true. Every eviction
  advances the generation, so that a monitor that was being computed while
  its inputs changed is not cached (see fetch)."
  [state pred]
  (-> state
      (update-in [:entries] #(into {} (remove (comp pred val)) %))
      (update-in [:generation] inc)))

(defn- item-account-ids
  [[before after]]
  (->> [before after]
       (mapcat :transaction/items)
       (keep (comp :id :transaction-item/account))
       set))

(defn- eviction-pred
  "Returns a predicate that identifies the entries to be evicted because of
  msg, or nil if there are none."
  [msg]
  (if (map? msg)
    (when-let [change (::prop/change msg)]
      (when (= :transaction (util/entity-type (some identity change)))
        (let [ids (item-account-ids change)]
          (when (seq ids)
            #(seq (intersection ids (:account-ids %)))))))
    (constantly true)))

(defn- handle
  [state msg]
  (if-let [ack (::ack msg)]
    (a/put! ack true)
    (when-let [pred (eviction-pred msg)]
      (swap! state evict pred))))

(defmethod ig/init-key ::cache
  ; Subscribes to the changes that make cached monitors stale and evicts
  ; them on a background thread.
  [_ {:keys [publisher ttl-minutes] :or {ttl-minutes 60}}]
  (let [id (keyword (name ::flush) (str (random-uuid)))
        ch (entities/sub publisher (conj eviction-topics id))
        state (atom {:entries {}
                     :generation 0})]
    {:ch ch
     :id id
     :publisher publisher
     :state state
     :ttl-millis (* ttl-minutes 60 1000)
     :done (a/thread
             (loop []
               (when-let [msg (a/<!! ch)]
                 (try
                   (handle state msg)
                   (catch Throwable e
                     ; a monitor that isn't evicted is stale until it
                     ; expires, so clear them all
                     (log/error e "[monitor-cache] unable to evict monitors")
                     (swap! state evict (constantly true))))
                 (recur))))}))

(defmethod ig/halt-key! ::cache
  [_ {:keys [ch publisher done]}]
  (entities/unsub publisher ch)
  (a/<!! done))

(defn await-evictions
  "Blocks until the cache has handled every change published before the
  call, or until the timeout (in milliseconds, default 10 seconds) elapses.
  Returns true if the changes were handled, false if it timed out."
  ([cache] (await-evictions cache {}))
  ([{:keys [publisher id]} {:keys [timeout-ms] :or {timeout-ms 10000}}]
   (let [ack (a/promise-chan)]
     (entities/signal publisher id {::ack ack})
     (boolean (first (a/alts!! [ack (a/timeout timeout-ms)]))))))

(defn- prune
  "Removes the entries that are expired, or that are for another day."
  [entries as-of now]
  (into {}
        (remove (fn [[[_ date] {:keys [expires-at]}]]
                  (or (not= as-of date)
                      (<= expires-at now))))
        entries))

(defn- store
  [state k entry generation]
  (if (= generation (:generation state))
    (update-in state [:entries] #(-> %
                                     (prune (second k) (now-millis))
                                     (assoc k entry)))
    state))

(defn fetch
  "Returns the budget monitor for the account as of today, from the cache
  if it's there, or else computed and cached. Without a cache, the monitor
  is simply computed."
  [{:keys [state ttl-millis] :as cache} account]
  (if cache
    (let [k [(:id account) (t/local-date)]
          {:keys [entries generation]} @state
          {:keys [monitor expires-at]} (get entries k)]
      (if (and monitor (< (now-millis) expires-at))
        monitor
        (let [monitor (rpt/monitor account (second k))
              entry {:monitor monitor
                     :account-ids (rpt/monitored-account-ids account)
                     :expires-at (+ (now-millis) ttl-millis)}]
          (swap! state store k entry generation)
          monitor)))
    (rpt/monitor account)))
