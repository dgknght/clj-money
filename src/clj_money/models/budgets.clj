(ns clj-money.models.budgets
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [clj-time.core :as t]
            [clj-time.coerce :refer [to-local-date
                                     to-date-time]]
            [clj-time.periodic :refer [periodic-seq]]
            [clj-money.util :refer [to-sql-date]]
            [clj-money.coercion :as coercion]
            [clj-money.validation :as validation]
            [clj-money.authorization :as authorization]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.helpers :refer [with-storage
                                              with-transacted-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [create-budget
                                              update-budget
                                              create-budget-item
                                              update-budget-item
                                              find-budget-by-date
                                              find-budget-item-by-id
                                              select-budgets
                                              select-budget-items-by-budget-id
                                              delete-budget
                                              delete-budget-item]])
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
      (authorization/tag-resource :budget-item)))

(defn- select-items-by-budget-id
  [storage budget-id]
  (map after-item-read
       (select-budget-items-by-budget-id storage budget-id)))

(defn- after-read
  [storage budget]
  (when budget
    (-> budget
        (authorization/tag-resource :budget)
        (update-in [:start-date] to-local-date)
        (update-in [:end-date] to-local-date)
        (update-in [:period] keyword)
        (assoc :items (select-items-by-budget-id storage (:id budget))))))

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
  [_ budget]
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
  [_ budget]
  (-> budget
      (assoc :end-date (to-sql-date (end-date budget)))
      (update-in [:start-date] to-sql-date)
      (update-in [:period] name)))

(defn- period-count-must-be-greater-than-one
  [{period-count :period-count :as budget}]
  {:model budget
   :errors (if (and period-count
                    (> 1 period-count))
             [[:period-count "Period count must be greater than zero"]]
             [])})

(def create
  (create-fn {:before-save before-save
              :create create-budget
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
     (->> (select-budgets s criteria options)
          (map #(after-read s %))))))

(defn find-by-id
  "Returns the specified budget"
  [storage-spec id]
  (with-storage [s storage-spec]
    (->> (select-budgets s {:id id} {:limit 1})
         first
         (after-read s))))

(defn find-by-date
  "Returns the budget containing the specified date"
  [storage-spec entity-id date]
  (with-storage [s storage-spec]
    (->> (find-budget-by-date s entity-id (to-sql-date date))
         (after-read s))))

(defn reload
  "Returns the lastest version of the specified budget from the data store"
  [storage-spec budget]
  (with-storage [s storage-spec]
    (->> budget
         :id
         (find-by-id s)
         (after-read s))))

(def update
  (update-fn {:spec ::existing-budget
              :before-validation before-validation
              :before-save before-save
              :coercion-rules coercion-rules
              :update update-budget
              :find find-by-id}))

(defn find-item-by-id
  "Returns the budget item with the specified id"
  [storage-spec item-id]
  (with-storage [s storage-spec]
    (->> item-id
         (find-budget-item-by-id s)
         after-item-read)))

(defn find-item-by-account
  "Finds the item in the specified budget associated with the specified account"
  [budget account-or-id]
  (let [account-id (or (:id account-or-id)
                       account-or-id)]
    (->> budget
         :items
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

(defn- before-item-validation
  [item]
  item)

(defn- validate-item
  [storage spec budget item]
  (->> item
       before-item-validation
       (validation/validate spec (item-validation-rules storage budget))))

(defn- before-save-item
  [item]
  (update-in item [:periods] (comp prn-str vec)))

(defn create-item
  "Adds a new budget item to an existing budget"
  [storage-spec item]
  (with-storage [s storage-spec]
    (let [budget (when (:budget-id item) (find-by-id s (:budget-id item)))
          validated (validate-item s ::new-budget-item budget item)]
      (if (validation/has-error? validated)
        validated
        (->> validated
             before-save-item
             (create-budget-item s)
             after-item-read)))))

(defn- reload-item
  "Returns the lastest version of the specified budget from the data store"
  [storage-spec item]
  (with-storage [s storage-spec]
    (->> item
         :id
         (find-item-by-id s))))

(defn update-item
  "Updates the specified budget item"
  [storage-spec item]
  (with-storage [s storage-spec]
    (let [item (coercion/coerce item item-coercion-rules)
          budget (->> item
                      :id
                      (find-item-by-id s)
                      :budget-id
                      (find-by-id s))
          validated (validate-item s
                                   ::existing-budget-item
                                   budget
                                   (select-keys item [:id
                                                      :account-id
                                                      :periods]))]
      (if (validation/valid? validated)
        (do
          (->> validated
               before-save-item
               (update-budget-item s))
          (reload-item s validated))
        validated))))

(defn delete
  "Removes the specified budget from the system"
  [storage-spec id]
  (with-storage [s storage-spec]
    (delete-budget s id)))

(defn delete-item
  "Removes the specified budget from the system"
  [storage-spec id]
  (with-storage [s storage-spec]
    (delete-budget-item s id)))

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
