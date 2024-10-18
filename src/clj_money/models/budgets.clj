(ns clj-money.models.budgets
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.walk :refer [keywordize-keys]]
            [java-time.api :as t]
            [config.core :refer [env]]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage
                                                   with-transacted-storage]]
            [dgknght.app-lib.core :refer [update-in-if
                                          assoc-if]]
            [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.validation :as v :refer [with-validation]]
            [clj-money.db :as db]
            [clj-money.dates :as dates]
            [clj-money.models :as models]
            [clj-money.models.accounts :as accounts]))

(defn- all-accounts-belong-to-budget-entity?
  [{:budget/keys [entity items]}]
  (if (seq items)
    (when entity
      (when-let [account-ids (->> items
                                  (map :budget-item/account)
                                  (filter identity)
                                  seq)]
        (let [entities (->> (models/select
                              (db/model-type
                                {:id [:in account-ids]}
                                :account))
                            (map :account/entity)
                            set)]
          (and (= 1 (count entities))
               (entities entity)))))
    true))

(v/reg-spec all-accounts-belong-to-budget-entity?
            {:message "All accounts must belong to the budget entity"
             :path [:budget/items]})

(defn- period-counts-match?
  [{:budget/keys [items period-count]}]
  (if (seq items)
    (let [item-period-counts (->> items
                                  (map (comp count :budget/periods))
                                  set)]
      (and (= 1 (count item-period-counts))
           (item-period-counts period-count)))
    true))

(v/reg-spec period-counts-match?
            {:message "All items must have a number of periods that matches the budget period count"
             :path [:budget/items]})

(s/def :budget-item/periods (s/coll-of decimal? :min-count 1))
(s/def :budget-item/account ::models/model-ref)
(s/def :budget-item/spec (s/nilable map?))
(s/def ::models/budget-item (s/keys :req [:budget-item/account
                                          :budget-item/periods]
                                    :opt [:budget-item/spec]))

(s/def :budget/items (s/coll-of ::models/budget-item))
(s/def :budget/name v/non-empty-string?)
(s/def :budget/start-date t/local-date?)
(s/def :budget/period #{:week :month})
(s/def :budget/period-count v/positive-integer?)
(s/def :budget/entity ::models/model-ref)
(s/def ::models/budget (s/and (s/keys :req [:budget/name
                                            :budget/start-date
                                            :budget/period
                                            :budget/period-count
                                            :budget/entity]
                                      :opt [:budget/items])
                              #_all-accounts-belong-to-budget-entity?
                              #_period-counts-match?))

(defn default-start-date
  []
  (let [now (t/instant)]
    (t/local-date (+ 1 (t/year now)) 1 1)))

(defn- after-item-read
  [item]
  (-> item
      (update-in [:spec] #(when % (keywordize-keys %)))
      (update-in-if [:spec :average] bigdec)
      (tag ::models/budget-item)))

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
         (update-in [:start-date] t/local-date)
         (update-in [:end-date] t/local-date)
         (update-in [:period] keyword)))))

(def period-map
  {:month (t/months 1)
   :week (t/weeks 1)
   :quarter (t/months 3)})

(defn period-seq
  "Returns a sequence of the java.time.Period instances in the budget based on
  :start-date, :period, :period-count"
  [{:as budget :budget/keys [start-date period-count]}]
  (when budget
    (->> (dates/periodic-seq start-date
                             (get-in period-map
                                     [(:period budget)]
                                     (t/months 1)))
         (partition 2 1)
         (map-indexed (fn [index [start next-start]]
                        {:start start
                         :end (t/minus next-start (t/days 1))
                         :index index
                         :interval (t/period start next-start)}))
         (take period-count))))

(defn end-date
  [budget]
  (-> budget
      period-seq
      last
      :end
      t/local-date))

(defmethod models/before-save :budget
  [budget]
  (assoc budget :budget/end-date (end-date budget)))

(defn search
  "Returns a list of budgets matching the specified criteria"
  ([criteria]
   (search criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (map #(after-read % options)
          (storage/select (tag criteria ::models/budget)
                          options)))))

(defn ^:deprecated find-by
  ([criteria]
   (find-by criteria {}))
  ([criteria options]
   (first (search criteria (merge {:include-items? true}
                                  options
                                  {:limit 1})))))

(defn ^:deprecated find
  "Returns the specified budget"
  [budget-or-id]
  (throw (UnsupportedOperationException. "find is deprecated")))

(defn find-by-date
  "Returns the budget containing the specified date"
  [entity date]
  (models/find-by #:budget{:start-date [:<= date]
                           :end-date [:>= date]
                           :entity entity}))

(defn ^:deprecated reload
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

(defn ^:deprecated update
  [_budget]
  (throw (UnsupportedOperationException. "Use models/put instead"))

  #_(with-transacted-storage (env :db)
    (with-validation budget ::budget
      (-> budget
          before-save
          storage/update)
      (update-items budget)
      (find budget))))

(defn ^:deprecated find-item-by
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

(defn ^:deprecated create
  [_budget]
  (throw (UnsupportedOperationException. "Use models/put instead"))
  #_(with-transacted-storage (env :db)
    (with-validation budget ::budget
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
  (dates/within?
    date
    [(:start period)
     (:end period)]))

(defn period-containing
  "Returns the budget period containing the specified date

  This is a map containing :start-date, :end-date, :index, etc."
  [budget date]
  (->> (period-seq budget)
       (map-indexed #(assoc %2 :index %1))
       (filter #(within-period? % date))
       first))

(defn percent-of-period
  [budget as-of]
  (let [period (period-containing budget as-of)
        days-in-period (inc (dates/days-between (:start period) (:end period)))
        days (inc (dates/days-between (:start period)
                                      as-of))]
    (with-precision 5 (/ days days-in-period))))
