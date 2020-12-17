(ns clj-money.models.budgets
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clj-time.core :as t]
            [clj-time.coerce :refer [to-local-date
                                     to-date-time]]
            [clj-time.periodic :refer [periodic-seq]]
            [environ.core :refer [env]]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage
                                                   with-transacted-storage]]
            [clj-money.validation :as validation :refer [with-validation]]
            [clj-money.util :refer [assoc-if
                                    ->id]]
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
  (tag item ::models/budget-item))

(defn- select-items
  ([criteria]
   (select-items criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (mapv after-item-read
           (storage/select (tag criteria ::models/budget-item)
                           options)))))

(defn- after-read
  ([budget] (after-read budget {}))
  ([budget {:keys [include-items?]}]
   (when budget
     (-> budget
         (tag ::models/budget)
         (assoc-if :items (when include-items? (select-items {:budget-id (:id budget)})))
         (update-in [:start-date] to-local-date)
         (update-in [:end-date] to-local-date)
         (update-in [:period] keyword)))))

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
  [budget]
  (-> budget
      (tag ::models/budget)
      (update-in [:period] name)
      (assoc :end-date (end-date budget))
      (dissoc :items)))

(defn search
  "Returns a list of budgets matching the specified criteria"
  ([criteria]
   (search criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (map #(after-read % options)
          (storage/select (tag criteria ::models/budget)
                          options)))))

(defn find-by
  ([criteria]
   (find-by criteria {}))
  ([criteria options]
   (first (search criteria (merge {:include-items? true}
                                  options
                                  {:limit 1})))))

(defn find
  "Returns the specified budget"
  [budget-or-id]
  (find-by {:id (->id budget-or-id)}))

(defn find-by-date
  "Returns the budget containing the specified date"
  [entity-id date]
  (find-by {:start-date [:<= date]
            :end-date [:>= date]
            :entity-id entity-id}))

(defn reload
  "Returns the lastest version of the specified budget from the data store"
  [budget]
  (find budget))

(defn- update-items
  [{:keys [items] :as budget}]
  (when items
    (let [existing (select-items {:budget-id (:id budget)})
          current-ids (->> items
                           (map :id)
                           set)]
      (doseq [removed (remove #(current-ids (:id %)) existing)]
        (storage/delete removed))
      (doseq [item (->> items
                        (filter :id)
                        (map #(tag % ::models/budget-item)))]
        (storage/update item))
      (doseq [item (->> items
                        (remove :id)
                        (map #(tag % ::models/budget-item))
                        (map #(assoc % :budget-id (:id budget))))]
        (storage/create item)))))

(defn update
  [budget]
  {:pre [(sequential? (:items budget))]}

  (with-transacted-storage (env :db)
    (with-validation budget ::existing-budget []
      (-> budget
          before-save
          storage/update)
      (update-items budget)
      (find budget))))

(defn find-item-by
  "Returns the budget item with the specified id"
  ([criteria]
   (find-item-by criteria {}))
  ([criteria options]
   (first (select-items criteria (merge options {:limit 1})))))

(defn find-item-by-account
  "Finds the item in the specified budget associated with the specified account"
  [{:keys [items]} account]
  (->> items
       (filter #(= (:id account) (:account-id %)))
       first))

(defn find-items-by-account
  "Finds items in the specified match belonging to the specified account or its children."
  [{:keys [items]} {:keys [child-ids id]}]
  (let [ids (if (seq child-ids)
              (conj (into #{} child-ids) id)
              (->> (accounts/search {:id id}
                                    {:include-children? true})
                   (map :id)
                   (into #{})))]
    (filter #(ids (:account-id %))
            items)))

(defn create
  [budget]
  (with-transacted-storage (env :db)
    (with-validation budget ::new-budget []
      (let [created (-> budget
                        before-save
                        storage/create
                        after-read)]
        (assoc created :items (mapv (comp storage/create
                                          #(tag % ::models/budget-item)
                                          #(assoc % :budget-id (:id created)))
                                    (:items budget)))))))

(defn delete
  "Removes the specified budget from the system"
  [budget]
  (with-storage (env :db)
    (storage/delete budget)))

(defn delete-item
  "Removes the specified budget from the system"
  [item]
  (with-storage (env :db)
    (storage/delete item)))

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
