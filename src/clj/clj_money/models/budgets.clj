(ns clj-money.models.budgets
  (:refer-clojure :exclude [update])
  (:require [clojure.spec.alpha :as s]
            [clj-time.core :as t]
            [clj-time.coerce :refer [to-local-date
                                     to-date-time]]
            [clj-time.periodic :refer [periodic-seq]]
            [clj-money.util :refer [to-sql-date
                                    rev-args]]
            [clj-money.coercion :as coercion]
            [clj-money.validation :as validation]
            [clj-money.models :as models]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.helpers :refer [with-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :as storage])
  (:import (org.joda.time LocalDate
                          Months
                          Weeks
                          Days)))

(s/def ::id integer?)
(s/def ::name string?)
(s/def ::start-date (partial instance? LocalDate))
(s/def ::period #{:week :month :quarter})
(s/def ::period-count validation/positive-integer?)
(s/def ::entity-id integer?)
(s/def ::new-budget (s/keys :req-un [::name ::start-date ::period ::period-count ::entity-id]))
(s/def ::existing-budget (s/keys :req-un [::id ::name ::start-date ::period ::period-count] :opt-un [::entity-id]))
(s/def ::periods (s/coll-of decimal? :min-count 1))
(s/def ::account-id integer?)
(s/def ::budget-id integer?)
(s/def ::new-budget-item (s/keys :req-un [::account-id ::periods ::budget-id]))
(s/def ::existing-budget-item (s/keys :req-un [::id ::account-id ::periods] :opt-un [::budget-id]))

(defn default-start-date
  []
  (let [now (t/now)]
    (t/local-date (+ 1 (t/year now)) 1 1)))

(defn- after-item-read
  [item]
  (-> item
      (update-in [:periods] read-string)
      (models/tag ::models/budget-item)))

(defn- select-items
  ([storage criteria]
   (select-items storage criteria {}))
  ([storage criteria options]
   (map after-item-read
        (storage/select storage
                        (models/tag criteria :budget-item)
                        options))))

(defn- after-read
  [budget storage]
  (when budget
    (-> budget
        (models/tag ::models/budget)
        (update-in [:start-date] to-local-date)
        (update-in [:end-date] to-local-date)
        (update-in [:period] keyword)
        (assoc :items (select-items storage {:budget-id (:id budget)})))))

(def ^:private coercion-rules
  [(coercion/rule :integer [:id])
   (coercion/rule :integer [:entity-id])
   (coercion/rule :local-date [:start-date])
   (coercion/rule :keyword [:period])
   (coercion/rule :integer [:period-count])])

(def ^:private item-coercion-rules
  [(coercion/rule :integer [:budget-id])
   (coercion/rule :integer [:id])
   (coercion/rule :integer [:account-id])])

(defn- before-validation
  [budget & _]
  (dissoc budget :items))

(def period-map
  {:month Months/ONE
   :week Weeks/ONE
   :quarter Months/THREE})

(defn period-seq
  "Returns a sequence of the periods in the budget based on
  :start-date, :period, :period-count"
  [budget]
  (when budget
    (->> ((:period budget) period-map)
         (periodic-seq (:start-date budget))
         (partition 2 1)
         (map-indexed (fn [index [start next-start]]
                        {:start start
                         :end (t/minus next-start Days/ONE)
                         :index index
                         :interval (t/interval (to-date-time start)
                                               (to-date-time next-start))}))
         (take (:period-count budget)))))

(defn end-date
  [budget]
  (-> budget
      period-seq
      last
      :end
      to-local-date))

(defn- before-save
  [budget & _]
  (-> budget
      (models/tag :budget)
      (assoc :end-date (to-sql-date (end-date budget)))
      (update-in [:start-date] to-sql-date)
      (update-in [:period] name)))

(def create
  (create-fn {:before-save before-save
              :create (rev-args storage/create)
              :spec ::new-budget
              :before-validation before-validation
              :coercion-rules coercion-rules
              :after-read after-read}))

(defn search
  "Returns a list of budgets matching the specified criteria"
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (->> (storage/select s (models/tag criteria :budget) options)
          (map #(after-read % s))))))

(defn find-by
  ([storage-spec criteria]
   (find-by storage-spec criteria {}))
  ([storage-spec criteria options]
   (first (search storage-spec criteria (merge options {:limit 1})))))

(defn find-by-id
  "Returns the specified budget"
  [storage-spec id]
  (find-by storage-spec {:id id}))

(defn find-by-date
  "Returns the budget containing the specified date"
  [storage-spec entity-id date]
  (find-by storage-spec {:start-date [:<= date]
                         :end-date [:>= date]
                         :entity-id entity-id}))

(defn reload
  "Returns the lastest version of the specified budget from the data store"
  [storage-spec budget]
  (find-by-id storage-spec (:id budget)))

(def update
  (update-fn {:spec ::existing-budget
              :before-validation before-validation
              :before-save before-save
              :coercion-rules coercion-rules
              :update (rev-args storage/update)
              :find find-by-id}))

(defn find-item-by
  "Returns the budget item with the specified id"
  ([storage-spec criteria]
   (find-item-by storage-spec criteria {}))
  ([storage-spec criteria options]
   (first (select-items storage-spec criteria (merge options {:limit 1})))))

(defn find-item-by-account
  "Finds the item in the specified budget associated with the specified account"
  [budget account-or-id]
  (let [account-id (or (:id account-or-id)
                       account-or-id)]
    (->> (:items budget)
         (filter #(= account-id (:account-id %)))
         first)))

(defn- budget-item-account-belongs-to-budget-entity
  [storage budget item]
  (if-let [account (accounts/find-by-id storage (:account-id item))]
    (= 1 (->> [account budget]
              (map :entity-id)
              (into #{})
              count))
    false))

(defn- budget-item-has-correct-number-of-periods
  [budget item]
  (= (:period-count budget) (count (:periods item))))

(defn- budget-item-account-is-unique?
  [budget item]
  (->> (:items budget)
       (filter #(= (:account-id item) (:account-id %)))
       (remove #(= (:id item) (:id %)))
       empty?))

(defn- item-validation-rules
  [storage budget]
  [(validation/create-rule (partial budget-item-account-is-unique? budget)
                           [:acount-id]
                           "Account is already in the budget")
   (validation/create-rule (partial budget-item-account-belongs-to-budget-entity storage budget)
                           [:account-id]
                           "Account must belong to the same entity as the budget")
   (validation/create-rule (partial budget-item-has-correct-number-of-periods budget)
                           [:periods]
                           "Number of periods must match the budget \"Period count\" value")])

(defn- validate-item
  [item storage spec budget]
  (validation/validate item spec (item-validation-rules storage budget)))

(defn- before-save-item
  [item]
  (-> item
      (models/tag :budget-item)
      (update-in [:periods] (comp prn-str vec))))

(defn create-item
  "Adds a new budget item to an existing budget"
  [storage-spec item]
  (with-storage [s storage-spec]
    (let [budget (when (:budget-id item) (find-by-id s (:budget-id item)))
          validated (validate-item item s ::new-budget-item budget)]
      (if (validation/has-error? validated)
        validated
        (->> validated
             before-save-item
             (storage/create s)
             after-item-read)))))

(defn- reload-item
  "Returns the lastest version of the specified budget from the data store"
  [storage-spec item]
  (find-item-by storage-spec {:id (:id item)}))

(defn update-item
  "Updates the specified budget item"
  [storage-spec item]
  (with-storage [s storage-spec]
    (let [item (coercion/coerce item item-coercion-rules)
          budget (find-by-id s (:budget-id item))
          validated (-> item
                        (select-keys [:id
                                      :account-id
                                      :periods])
                        (validate-item s
                                       ::existing-budget-item
                                       budget)) ]
      (if (validation/valid? validated)
        (do
          (->> validated
               before-save-item
               (storage/update s))
          (reload-item s validated))
        validated))))

(defn delete
  "Removes the specified budget from the system"
  [storage-spec budget]
  (with-storage [s storage-spec]
    (storage/delete s budget)))

(defn delete-item
  "Removes the specified budget from the system"
  [storage-spec id]
  (with-storage [s storage-spec]
    (storage/delete s id)))

(defn- within-period?
  "Returns a boolean value indicating whether or not
  the specified date is in the specified period"
  [period date]
  (t/within?
    (to-date-time (:start period))
    (to-date-time (:end period))
    (to-date-time date)))

(defn period-containing
  "Returns the budget period containing the specified date

  This is a map containing :start-date, :end-date, :index, etc."
  [budget date]
  (->> (map-indexed #(assoc %2 :index %1) (period-seq budget))
       (filter #(within-period? % (to-date-time date)))
       first))

(defn percent-of-period
  [budget as-of]
  (let [period (period-containing budget as-of)
        days-in-period (t/in-days (:interval period))
        days (+ 1 (t/in-days (t/interval (to-date-time (:start period))
                                         (to-date-time as-of))))]
    (with-precision 5 (/ days days-in-period))))
