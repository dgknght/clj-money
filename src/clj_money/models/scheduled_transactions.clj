(ns clj-money.models.scheduled-transactions
  (:refer-clojure :exclude [find update])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.validation :as v]
            [clj-money.models :as models]
            [clj-money.models.transactions :as trans]))

(defn- debit-credit-balanced?
  [items]
  (let [totals (->> items
                    (group-by :scheduled-transaction-item/action)
                    (map (fn [entry]
                           (update-in entry
                                      [1]
                                      #(->> %
                                            (map :scheduled-transaction-item/quantity)
                                            (reduce + 0M)))))
                    (into {}))]
    (= (:debit totals)
       (:credit totals))))

(v/reg-msg debit-credit-balanced? "The sum of debits must equal the sum of credits")

(defn- greater-than-zero?
  [n]
  (< 0 n))
(v/reg-msg greater-than-zero? "%s must be greater than zero")

(s/def :scheduled-transaction-item/action trans/actions)
(s/def :scheduled-transaction-item/account ::models/model-ref)
(s/def :scheduled-transaction-item/quantity v/positive-big-dec?)
(s/def ::models/scheduled-transaction-item (s/keys :req [:scheduled-transaction-item/action
                                                         :scheduled-transaction-item/account
                                                         :scheduled-transaction-item/quantity]))

(s/def :scheduled-transaction/items (s/and (s/coll-of ::models/scheduled-transaction-item
                                                      :min-count 2)
                                           debit-credit-balanced?))
(s/def :scheduled-transaction/entity ::models/model-ref)
(s/def :scheduled-transaction/description string?)
(s/def :scheduled-transaction/interval-type #{:day :week :month :year})
(s/def :scheduled-transaction/interval-count (s/and integer?
                                                    greater-than-zero?))
(s/def :scheduled-transaction/start-date t/local-date?)
(s/def :scheduled-transaction/end-date (s/nilable t/local-date?))
(s/def :scheduled-transaction/date-spec map?)
(s/def ::models/scheduled-transaction (s/keys :req [:scheduled-transaction/entity
                                                    :scheduled-transaction/description
                                                    :scheduled-transaction/interval-type
                                                    :scheduled-transaction/interval-count
                                                    :scheduled-transaction/start-date
                                                    :scheduled-transaction/date-spec
                                                    :scheduled-transaction/items]
                                              :opt [:scheduled-transaction/end-date]))

#_(defn- after-read
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

(defn ^:deprecated create
  [_sched-tran]
  (throw (UnsupportedOperationException. "create is deprecated"))
  #_(with-transacted-storage (env :db)
    (with-validation sched-tran ::scheduled-transaction
      (let [created (-> sched-tran
                        before-save
                        storage/create
                        after-read)]
        (assoc created :items (mapv (comp create-item
                                          #(assoc % :scheduled-transaction-id (:id created)))
                                    (:items sched-tran)))))))

(defn ^:deprecated search
  ([criteria]
   (search criteria {}))
  ([_criteria _options]
   (throw (UnsupportedOperationException. "search is deprecated"))))

(defn ^:deprecated find
  [_model-or-id]
  (throw (UnsupportedOperationException. "find is deprecated")))

#_(defn- update-items
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

(defn ^:deprecated update
  [_sched-tran]
  (throw (UnsupportedOperationException. "update is deprecated")))

(defn ^:deprecated delete
  "Removes the scheduled transaction from the system"
  [_sched-tran]
  (throw (UnsupportedOperationException. "delete is deprecated")))

(defn ^:deprecated realize
  "Creates new transactions based on the scheduled transaction,
  if the date of the new transactions would be within one week
  of the current date"
  [_sched-tran]
  (throw (UnsupportedOperationException. "realize is deprecated"))
  #_(let [transactions (->> (st/next-transaction-dates sched-tran)
                          (mapv #(-> sched-tran
                                     (st/->transaction %)
                                     trans/create)))]
    (when-let [transaction-date (:transaction-date (last transactions))]
      (update (assoc sched-tran :last-occurrence transaction-date)))
    transactions))
