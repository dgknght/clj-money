(ns clj-money.models.budgets
  (:refer-clojure :exclude [update])
  (:require [clojure.spec.alpha :as s]
            [clj-time.core :as t]
            [clj-time.coerce :refer [to-local-date
                                     to-date-time]]
            [clj-time.periodic :refer [periodic-seq]]
            [stowaway.core :as storage :refer [with-storage
                                               with-transacted-storage]]
            [clj-money.coercion :as coercion]
            [clj-money.validation :as validation :refer [with-validation]]
            [clj-money.models :as models]
            [clj-money.models.accounts :as accounts])
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
  (storage/tag item ::models/budget-item))

(defn- select-items
  ([storage criteria]
   (select-items storage criteria {}))
  ([storage criteria options]
   (mapv after-item-read
         (storage/select storage
                         (storage/tag criteria ::models/budget-item)
                         options))))

(defn- after-read
  [budget]
  (when budget
    (-> budget
        (storage/tag ::models/budget)
        (update-in [:start-date] to-local-date)
        (update-in [:end-date] to-local-date)
        (update-in [:period] keyword))))

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

(def period-map
  {:month Months/ONE
   :week Weeks/ONE
   :quarter Months/THREE})

(defn period-seq
  "Returns a sequence of the periods in the budget based on
  :start-date, :period, :period-count"
  [budget]
  (when budget
    (->> (periodic-seq (:start-date budget)
                       (get-in period-map
                               [(:period budget)]
                               Months/ONE))
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
      (storage/tag ::models/budget)
      (update-in [:period] name)
      (assoc :end-date (end-date budget))
      (dissoc :items)))

(defn- creation-rules
  [_storage]
  [])

(defn- append-items
  [budget storage options]
  (if (:include-items? options)
    (assoc budget :items (select-items storage {:budget-id (:id budget)}))
    budget))

(defn search
  "Returns a list of budgets matching the specified criteria"
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (map (comp #(append-items % s options)
                after-read)
          (storage/select s
                          (storage/tag criteria ::models/budget)
                          options)))))

(defn find-by
  ([storage-spec criteria]
   (find-by storage-spec criteria {}))
  ([storage-spec criteria options]
   (first (search storage-spec criteria (merge {:include-items? true}
                                               options
                                               {:limit 1})))))

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

(defn- update-items
  [storage {:keys [items] :as budget}]
  (let [existing (select-items storage {:budget-id (:id budget)})
        current-ids (->> items
                         (map :id)
                         set)]
    (doseq [removed (remove #(current-ids (:id %)) existing)]
      (storage/delete storage removed))
    (doseq [item (->> items
                      (filter :id)
                      (map #(storage/tag % ::models/budget-item)))]
      (storage/update storage item))
    (doseq [item (->> items
                      (remove :id)
                      (map #(storage/tag % ::models/budget-item))
                      (map #(assoc % :budget-id (:id budget))))]
      (storage/create storage item))))

(defn update
  [storage-spec budget]
  {:pre [(sequential? (:items budget))]}

  (with-storage [s storage-spec]
    (with-validation budget ::existing-budget []
      (as-> budget b
        (before-save b)
        (storage/update s b))
      (update-items s budget)
      (find-by-id s (:id budget)))))

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

#_(defn- item-rules
  [storage budget]
  [(validation/create-rule
     (partial budget-item-account-is-unique? budget)
     [:acount-id]
     "Account is already in the budget")
   (validation/create-rule
     (partial budget-item-account-belongs-to-budget-entity storage budget)
     [:account-id]
     "Account must belong to the same entity as the budget")
   (validation/create-rule
     (partial budget-item-has-correct-number-of-periods budget)
     [:periods]
     "Number of periods must match the budget \"Period count\" value")])

(defn create
  [storage-spec budget]
  (with-transacted-storage [s storage-spec]
    (with-validation budget ::new-budget []
      (let [created (after-read (storage/create s (before-save budget)))]
        (assoc created :items (mapv (comp #(storage/create s %)
                                          #(storage/tag % ::models/budget-item)
                                          #(assoc % :budget-id (:id created)))
                                    (:items budget)))))))

(defn- reload-item
  "Returns the lastest version of the specified budget from the data store"
  [storage-spec item]
  (find-item-by storage-spec {:id (:id item)}))

(defn update-item
  "Updates the specified budget item"
  [storage-spec item]
  #_(with-storage [s storage-spec]
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
