(ns clj-money.entities.budgets
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util :refer [id=]]
            [clj-money.dates :as dates]
            [clj-money.entities :as entities]
            [clj-money.entities.transaction-items :as trx-items]
            [clj-money.budgets :as budgets]))

(defn- accounts-belong-to-budget-entity?
  [{:budget/keys [items entity]}]
  (->> items
       (map :budget-item/account)
       set
       (map entities/resolve-ref)
       (every? #(id= (:account/entity %)
                     entity))))

(v/reg-spec accounts-belong-to-budget-entity?
            {:message "Account must belong to the same entity as the budget"
             :path [:budget/items]})

(defn- period-counts-match?
  [{:budget/keys [period items]}]
  (every? #(= (count (:budget-item/periods %))
              (first period))
          items))

(v/reg-spec period-counts-match?
            {:message "Must have the number of periods specified by the budget"
             :path [:budget-item/periods]})

(s/def :budget/items (s/nilable (s/coll-of ::entities/budget-item)))
(s/def :budget/name v/non-empty-string?)
(s/def :budget/start-date t/local-date?)
(s/def :budget/period ::dates/period)
(s/def :budget/entity ::entities/entity-ref)
(s/def ::entities/budget (s/and (s/keys :req [:budget/name
                                            :budget/start-date
                                            :budget/period
                                            :budget/entity]
                                      :opt [:budget/items])
                              accounts-belong-to-budget-entity?
                              period-counts-match?))

(defmethod entities/before-save :budget
  [budget]
  (assoc budget :budget/end-date (budgets/end-date budget)))

(defn find-by-date
  "Returns the budget containing the specified date"
  [entity date]
  {:pre [entity
         (:id entity)
         date
         (t/local-date? date)]}
  (entities/find-by #:budget{:start-date [:<= date]
                           :end-date [:>= date]
                           :entity entity}
                  {:include #{:budget/items}}))

(defn find-items-by-account
  "Finds items in the specified budget belonging to the specified account or its children."
  [items {:account/keys [child-ids] account-id :id}]
  {:pre [(seq items)]}
  (let [ids (if (seq child-ids)
              (conj (into #{} child-ids) account-id)
              (->> (entities/select (util/entity-type
                                    {:id account-id}
                                    :account)
                                  {:include-children? true})
                   (map :id)
                   (into #{})))]
    (filter #(ids (get-in % [:budget-item/account :id]))
            items)))

(defn- historical-items
  [{:budget/keys [entity period]} start-date]
  (let [end-date (t/plus start-date
                         (dates/period period))]
    (entities/select (util/entity-type
                       {:transaction/entity entity
                        :transaction/transaction-date [:between> start-date end-date]
                        :account/type [:in #{:income :expense}]}
                       :transaction-item)
                     {:select-also [:transaction/transaction-date]})))

(defn- auto-create-items
  [{:budget/keys [period]
    :as budget}
   start-date]
  (let [end-date (t/plus start-date
                         (dates/period period))]
    (->> (historical-items budget start-date)
         (trx-items/polarize-quantities)
         (budgets/create-items-from-history
           budget
           start-date
           end-date)
         (map #(assoc % :budget-item/budget budget))
         entities/put-many)))

(defn append-auto-created-items
  "Given a budget and a start date, fetches historical transaction items
  for the budget's period and appends auto-created budget items derived
  from that history. Returns the budget unchanged if start-date is nil."
  [budget start-date]
  (cond-> budget
    start-date (assoc :budget/items
                      (auto-create-items budget
                                         start-date))))
