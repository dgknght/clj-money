(ns clj-money.models.scheduled-transactions
  (:refer-clojure :exclude [find update])
  (:require [clojure.spec.alpha :as s]
            [clojure.set :refer [difference
                                 rename-keys]]
            [environ.core :refer [env]]
            [stowaway.core :as stow :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage
                                                   with-transacted-storage]]
            [clj-money.validation :as v :refer [with-validation]]
            [clj-money.models :as models]
            [clj-money.models.transactions :as trans]
            [clj-money.scheduled-transactions :as st]
            [clj-money.util :refer [->id
                                    update-in-if]]))

(s/def ::action #{:credit :debit})
(s/def ::account-id integer?)
(s/def ::quantity v/positive-big-dec?)
(s/def ::item (s/keys :req-un [::action ::account-id ::quantity]))

(s/def ::items (s/coll-of ::item))
(s/def ::entity-id integer?)
(s/def ::description string?)
(s/def ::interval-type #{:day :week :month :year})
(s/def ::interval-count (s/and integer? #(> % 0)))
(s/def ::start-date v/local-date?)
(s/def ::end-date v/nilable-local-date?)
(s/def ::date-spec map?)
(s/def ::new-scheduled-transaction (s/keys :req-un [::entity-id
                                                    ::description
                                                    ::interval-type
                                                    ::interval-count
                                                    ::start-date
                                                    ::date-spec
                                                    ::items]
                                           :opt-un [::end-date]))

(s/def ::id integer?)
(s/def ::existing-scheduled-transaction (s/keys :req-un [::id ::items]
                                                :opt-un [::description
                                                         ::interval-type
                                                         ::interval-count
                                                         ::start-date
                                                         ::date-spec]))

(declare search-items)

(defn- before-save
  [scheduled-transaction]
  (-> scheduled-transaction
      (update-in-if [:interval-type] name)
      (dissoc :items)
      (tag ::models/scheduled-transaction)))

(defn- before-item-save
  [item]
  (-> item
      (update-in-if [:action] name)
      (tag ::models/scheduled-transaction-item)))

(defn- after-read
  [scheduled-transaction]
  (-> scheduled-transaction
      (update-in [:interval-type] keyword)
      (update-in [:date-spec] rename-keys {"month" :month
                                           "day" :day
                                           "days" :days})
      (update-in-if [:date-spec :day] #(if (string? %)
                                         (keyword %)
                                         %))
      (update-in-if [:date-spec :days] #(map keyword %))
      (assoc :items (search-items {:scheduled-transaction-id (:id scheduled-transaction)}))
      (tag ::models/scheduled-transaction)))

(defn- after-item-read
  [item]
  (-> item
      (update-in [:action] keyword)
      (tag ::models/scheduled-transaction-item)))

(defn- at-least-two-items?
  [scheduled-transaction]
  (<= 2 (count (:items scheduled-transaction))))

(defn- debit-credit-balanced?
  [scheduled-transaction]
  (let [totals (->> (:items scheduled-transaction)
                    (group-by :action)
                    (map (fn [entry]
                           (update-in entry [1] #(->> %
                                                      (map :quantity)
                                                      (reduce +)))))
                    (into {}))]
    (= (:debit totals)
       (:credit totals))))

(def ^:private validation-rules
  [(v/create-rule at-least-two-items?
                  [:items]
                  "There must be at least two items")
   (v/create-rule debit-credit-balanced?
                  [:items]
                  "The sum of debits must equal the sum of credits")])

(defn- create-item
  [item]
  (-> item
      before-item-save
      storage/create
      after-item-read))

(defn create
  [sched-tran]
  (with-transacted-storage (env :db)
    (with-validation sched-tran ::new-scheduled-transaction validation-rules
      (let [created (-> sched-tran
                        before-save
                        storage/create
                        after-read)]
        (assoc created :items (mapv (comp create-item
                                          #(assoc % :scheduled-transaction-id (:id created)))
                                    (:items sched-tran)))))))

(defn search-items
  ([criteria]
   (search-items criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (map after-item-read
          (storage/select (tag criteria ::models/scheduled-transaction-item)
                          options)))))

(defn search
  ([criteria]
   (search criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (map after-read
          (storage/select (tag criteria ::models/scheduled-transaction)
                          options)))))

(defn find-by
  [criteria]
  (first (search criteria {:limit 1})))

(defn find
  [model-or-id]
  (find-by {:id (->id model-or-id)}))

(defn- update-items
  [{:keys [items id]}]
  (let [existing (find id)
        existing-ids (->> (:items existing)
                          (map :id)
                          (into #{}))]
    (doseq [item (remove :id items)]
      (create-item (assoc item :scheduled-transaction-id id)))
    (when-let [removed-ids (seq (difference existing-ids
                                            (->> items
                                                 (filter :id)
                                                 (map :id)
                                                 (into #{}))))]
      (doseq [id removed-ids]
        (storage/delete (tag {:id id} ::models/scheduled-transaction-item))))
    (doseq [item (filter :id items)]
      (-> item
          before-item-save
          storage/update))))

(defn update
  [sched-tran]
  (with-transacted-storage (env :db)
    (with-validation sched-tran ::existing-scheduled-transaction validation-rules
      (update-items sched-tran)
      (-> sched-tran
          before-save
          storage/update)
      (find sched-tran))))

(defn delete
  "Removes the scheduled transaction from the system"
  [sched-tran]
  (with-storage (env :db)
    (storage/delete sched-tran)))

(defn realize
  "Creates new transactions based on the scheduled transaction,
  if the date of the new transactions would be within one week
  of the current date"
  [sched-tran]
  (let [transactions (->> (st/next-transaction-dates sched-tran)
                          (mapv #(-> sched-tran
                                     (st/->transaction %)
                                     trans/create)))]
    (when-let [transaction-date (:transaction-date (last transactions))]
      (update (assoc sched-tran :last-occurrence transaction-date)))
    transactions))
