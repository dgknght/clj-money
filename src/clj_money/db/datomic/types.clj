(ns clj-money.db.datomic.types
  (:require [clojure.walk :refer [prewalk postwalk]]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [java-time.api :as t]
            [clj-money.dates :as dates]))

(derive java.lang.Integer ::integer)
(derive java.lang.Long ::integer)
(derive java.lang.String ::string)
(derive clojure.lang.PersistentVector ::vector)

(defmulti coerce-id type)

(defmethod coerce-id ::integer [id] id)

(defmethod coerce-id ::string
  [id]
  (Long/parseLong id))

(defmethod coerce-id ::vector
  [v]
  (mapv (fn [x]
          (if (string? x)
            (coerce-id x)
            x))
        v))

(defn ->java-date-time
  [local-date-time]
  (t/java-date
    (t/zoned-date-time local-date-time
                       (t/zone-offset 0 0))))

(defn ->java-date
  [local-date]
  (t/java-date
    (t/zoned-date-time local-date
                       (t/local-time 0 0 0 0)
                       (t/zone-offset 0 0))))

(defn ->java-dates
  "Given a entity or criteria, replace all local-date instances with java dates"
  [m]
  (postwalk (fn [x]
              (cond
                (t/local-date-time? x) (->java-date-time x)
                (t/local-date? x)      (->java-date x)
                (t/instant? x)         (t/java-date x)
                :else x))
            m))

(def ^:private coercions
  {:account/system-tags set
   :account/user-tags set
   :account/allocations (partial into {})
   :account/transaction-date-range (partial mapv dates/->local-date)
   :budget/start-date dates/->local-date
   :budget/end-date dates/->local-date
   :budget-item-spec/start-date dates/->local-date
   :cached-price/trade-date dates/->local-date
   :entity/price-date-range (partial mapv dates/->local-date)
   :entity/transaction-date-range (partial mapv dates/->local-date)
   :lot/purchase-date dates/->local-date
   :price/trade-date dates/->local-date
   :reconciliation/end-of-period dates/->local-date
   :settings/monitored-accounts set
   :transaction/transaction-date dates/->local-date
   :scheduled-transaction/start-date dates/->local-date
   :scheduled-transaction/end-date dates/->local-date
   :scheduled-transaction/last-occurrence dates/->local-date
   :scheduled-transaction/date-spec read-string})

(defn apply-coercions
  [m]
  (postwalk (fn [x]
              (if (map-entry? x)
                (update-in x [1] (coercions (key x)
                                            identity))
                x))
            m))

(defn- datomize-ref-val
  [x]
  (cond
    (map? x)
    (:id x)

    (sequential? x)
    (if (keyword? (first x))
      [(first x)
       (datomize-ref-val (second x))]
      (mapv datomize-ref-val x))

    :else x))

(defn- datomize*
  [{:keys [ref-keys]}]
  (fn [x]
    (cond
      (and (map-entry? x)
           (ref-keys (key x)))
      (update-in x [1] datomize-ref-val)

      :else x)))

(s/def ::ref-keys (s/coll-of keyword? :kind set?))
(s/def ::datomize-opts (s/keys :opt-un [::ref-keys]))

(defn datomize
  ([opts] #(datomize % opts))
  ([m opts]
   {:pre [(s/valid? ::datomize-opts opts)]}
   (prewalk (datomize* opts)
            m)))
