(ns clj-money.models.scheduled-transactions
  (:refer-clojure :exclude [find update])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [difference
                                 rename-keys]]
            [java-time.api :as t]
            [config.core :refer [env]]
            [stowaway.core :as stow :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage
                                                   with-transacted-storage]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.validation :as v :refer [with-validation]]
            [clj-money.models :as models]
            [clj-money.models.transactions :as trans]
            [clj-money.scheduled-transactions :as st]))

(defn- at-least-two-items?
  [items]
  (<= 2 (count items)))

(v/reg-msg at-least-two-items? "There must be at least two items")

(defn- debit-credit-balanced?
  [items]
  (let [totals (->> items
                    (group-by :action)
                    (map (fn [entry]
                           (update-in entry [1] #(->> %
                                                      (map :quantity)
                                                      (reduce +)))))
                    (into {}))]
    (= (:debit totals)
       (:credit totals))))

(v/reg-msg debit-credit-balanced? "The sum of debits must equal the sum of credits")

(def greater-than-zero?
  (every-pred integer? #(> % 0)))

(v/reg-msg greater-than-zero? "%s must be greater than zero")

(s/def ::action #{:credit :debit})
(s/def ::account-id integer?)
(s/def ::quantity v/positive-big-dec?)
(s/def ::item (s/keys :req-un [::action ::account-id ::quantity]))

(s/def ::items (s/and (s/coll-of ::item)
                      at-least-two-items?
                      debit-credit-balanced?))
(s/def ::entity-id integer?)
(s/def ::description string?)
(s/def ::interval-type #{:day :week :month :year})
(s/def ::interval-count greater-than-zero?)
(s/def ::start-date t/local-date?)
(s/def ::end-date (s/nilable t/local-date?))
(s/def ::date-spec map?)
(s/def ::scheduled-transaction (s/keys :req-un [::entity-id
                                                ::description
                                                ::interval-type
                                                ::interval-count
                                                ::start-date
                                                ::date-spec
                                                ::items]
                                       :opt-un [::end-date]))

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

(defn- create-item
  [item]
  (-> item
      before-item-save
      storage/create
      after-item-read))

(defn create
  [sched-tran]
  (with-transacted-storage (env :db)
    (with-validation sched-tran ::scheduled-transaction
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
  {:pre [(:id sched-tran)]}

  (with-transacted-storage (env :db)
    (with-validation sched-tran ::scheduled-transaction
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
