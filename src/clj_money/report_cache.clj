(ns clj-money.report-cache
  "Caches the balance sheet, income statement, and budget reports (see
  clj-money.reports), which are slow to compute.

  A report is cached by its type, its entity or budget, and its dates.
  Each cached report covers a range of transaction dates: a balance sheet
  everything up to its as-of date, an income statement its since and
  as-of dates, and a budget report its budget's start date to its as-of
  date. Cached reports are evicted:

  - when a change to a transaction has been propagated, for the reports of
    its entity whose range includes its transaction date (before or after
    the change). Reports are computed from propagated values, like the
    balances of the transaction items, so the eviction waits for the
    ::prop/propagated signal rather than the change itself.
  - when a price or a lot change has been propagated, for every report, as
    they change the value of commodity accounts.
  - when an account, budget, budget item or entity changes, for every
    report, as these change rarely.
  - after :ttl-minutes (default 60), to cover changes that aren't
    published, like imports and work done in the REPL."
  (:require [clojure.core.async :as a]
            [clojure.tools.logging :as log]
            [integrant.core :as ig]
            [java-time.api :as t]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.entities.propagation :as prop]
            [clj-money.reports :as rpt]))

(def ^:private eviction-topics
  #{:account :budget :budget-item :entity ::prop/propagated})

(def ^:private propagated-eviction-types
  #{:price :lot})

(defn- now-millis []
  (System/currentTimeMillis))

(defn- evict
  "Removes the entries for which (pred entry) is true. Every eviction
  advances the generation, so that a report that was being computed while
  its inputs changed is not cached (see fetch)."
  [state pred]
  (-> state
      (update-in [:entries] #(into {} (remove (comp pred val)) %))
      (update-in [:generation] inc)))

(defn- covers?
  "Returns true if the entry is for the entity of the transaction and its
  range includes the transaction date."
  [{:keys [entity-id since as-of]}
   {:transaction/keys [entity transaction-date]}]
  (and (= entity-id (:id entity))
       (not (and since (t/before? transaction-date since)))
       (not (and as-of (t/after? transaction-date as-of)))))

(defn- eviction-pred
  "Returns a predicate that identifies the entries to be evicted because of
  msg, or nil if there are none."
  [msg]
  (if (map? msg)
    (when-let [change (::prop/change msg)]
      (let [entity-type (util/entity-type (some identity change))]
        (cond
          (= :transaction entity-type)
          (let [trxs (filter identity change)]
            (fn [entry]
              (some #(covers? entry %) trxs)))

          (propagated-eviction-types entity-type)
          (constantly true))))
    (constantly true)))

(defn- handle
  [state msg]
  (if-let [ack (::ack msg)]
    (a/put! ack true)
    (when-let [pred (eviction-pred msg)]
      (swap! state evict pred))))

(defmethod ig/init-key ::cache
  ; Subscribes to the changes that make cached reports stale and evicts
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
                     ; a report that isn't evicted is stale until it
                     ; expires, so clear them all
                     (log/error e "[report-cache] unable to evict reports")
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
  "Removes the entries that are expired."
  [entries now]
  (into {}
        (remove (fn [[_ {:keys [expires-at]}]]
                  (<= expires-at now)))
        entries))

(defn- store
  [state k entry generation]
  (if (= generation (:generation state))
    (update-in state [:entries] #(-> %
                                     (prune (now-millis))
                                     (assoc k entry)))
    state))

(defn- fetch
  "Returns the report with the key k from the cache if it's there, or else
  computes it with (compute) and caches it with the scope (a map with the
  :entity-id, :since and :as-of that it covers). Without a cache, the report
  is simply computed."
  [{:keys [state ttl-millis] :as cache} k scope compute]
  (if cache
    (let [{:keys [entries generation]} @state
          {:keys [report expires-at]} (get entries k)]
      (if (and report (< (now-millis) expires-at))
        report
        (let [report (compute)]
          (swap! state
                 store
                 k
                 (assoc scope
                        :report report
                        :expires-at (+ (now-millis) ttl-millis))
                 generation)
          report)))
    (compute)))

(defn balance-sheet
  "Returns the balance sheet for the entity as of the date (see
  clj-money.reports/balance-sheet), from the cache if possible."
  [cache entity as-of]
  (fetch cache
         [:balance-sheet (:id entity) as-of]
         {:entity-id (:id entity)
          :as-of as-of}
         #(rpt/balance-sheet entity as-of)))

(defn income-statement
  "Returns the income statement for the entity for the dates (see
  clj-money.reports/income-statement), from the cache if possible."
  [cache entity since as-of]
  (fetch cache
         [:income-statement (:id entity) since as-of]
         {:entity-id (:id entity)
          :since since
          :as-of as-of}
         #(rpt/income-statement entity since as-of)))

(defn budget
  "Returns the budget report for the budget with the options (see
  clj-money.reports/budget), from the cache if possible."
  [cache budget {:keys [as-of] :as opts}]
  ; Without an :as-of, the report's date depends on today's, so today's
  ; date is part of the key, and the report covers the whole budget.
  (fetch cache
         [:budget (:id budget) opts (when-not as-of (t/local-date))]
         {:entity-id (-> budget :budget/entity :id)
          :since (:budget/start-date budget)
          :as-of (or as-of (:budget/end-date budget))}
         #(rpt/budget budget opts)))
