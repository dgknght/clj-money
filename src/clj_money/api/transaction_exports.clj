(ns clj-money.api.transaction-exports
  "Exports the transactions of an account as CSV, for the account's details
  on the Accounts page (issue #423)"
  (:require [clojure.data.csv :as csv]
            [clojure.string :as string]
            [ring.util.response :as res]
            [dgknght.app-lib.core :refer [parse-bool
                                          index-by]]
            [dgknght.app-lib.api :as api]
            [clj-money.dates :as dates]
            [clj-money.util :as util]
            [clj-money.accounts :refer [polarize-quantity]]
            [clj-money.entities :as entities]
            [clj-money.authorization :as auth :refer [+scope
                                                      authorize]]
            [clj-money.authorization.accounts]
            [clj-money.authorization.transaction-items]))

(def ^:private columns
  "The fields that can be exported, in the order they're written"
  [[:transaction-date "Date"]
   [:description "Description"]
   [:amount "Amount"]
   [:balance "Balance"]
   [:memo "Memo"]
   [:other-account "Other Account"]
   [:reconciled "Reconciled"]])

(def ^:private field-keys
  (set (map first columns)))

(defn- find-account
  [{:keys [params authenticated]}]
  (some-> {:id (:account-id params)}
          (+scope :account authenticated ::auth/manage)
          entities/find-by
          (authorize ::auth/show authenticated)))

(defn- parse-fields
  "Returns the requested fields in column order, or all of them when none are
  requested"
  [{:keys [fields]}]
  (let [requested (when-not (string/blank? fields)
                    (->> (string/split fields #",")
                         (map (comp keyword string/trim))
                         (filter field-keys)
                         set))]
    (if (seq requested)
      (filter (comp requested first) columns)
      columns)))

(defn- date-criterion
  "Since the last zero balance, the items are read through the end date and
  those through the zero balance are dropped afterward, as the zero balance
  may precede any start date"
  [{:keys [start-date end-date since-zero-balance]}]
  (let [start (when-not (parse-bool since-zero-balance)
                (some-> start-date dates/unserialize-local-date))
        end (some-> end-date dates/unserialize-local-date)]
    (cond
      (and start end) [:between start end]
      start [:>= start]
      end [:<= end])))

(defn- fetch-items
  [account {:keys [params authenticated]}]
  (entities/select (+scope (cond-> {:transaction-item/account account}
                             (date-criterion params)
                             (assoc :transaction/transaction-date
                                    (date-criterion params)))
                           :transaction-item
                           authenticated)
                   :select-also [:transaction/transaction-date
                                 :transaction/description
                                 :transaction-item/transaction]
                   :sort [[:transaction/transaction-date :asc]
                          [:transaction-item/index :asc]]))

(defn- drop-through-zero-balance
  "Drops the items through the last one that leaves the account with a zero
  balance"
  [items {:keys [since-zero-balance]}]
  (if (parse-bool since-zero-balance)
    (let [zero-balance? #(some-> % :transaction-item/balance zero?)]
      (if (some zero-balance? items)
        (->> (reverse items)
             (take-while (complement zero-balance?))
             reverse)
        items))
    items))

(defn- fetch-transactions
  [items]
  (if-let [ids (seq (distinct (map (comp :id :transaction-item/transaction)
                                   items)))]
    (index-by :id (entities/select {:id [:in (vec ids)]}
                                   :entity-type :transaction))
    {}))

(defn- fetch-account-names
  [transactions]
  (if-let [ids (->> (vals transactions)
                    (mapcat :transaction/items)
                    (map (comp :id :transaction-item/account))
                    distinct
                    seq)]
    (->> (entities/select {:id [:in (vec ids)]} :entity-type :account)
         (map (juxt :id :account/name))
         (into {}))
    {}))

(defn- fetch-completed-reconciliation-ids
  [items]
  (if-let [ids (->> items
                    (keep (comp :id :transaction-item/reconciliation))
                    distinct
                    seq)]
    (->> (entities/select {:id [:in (vec ids)]
                           :reconciliation/status :completed})
         (map :id)
         set)
    #{}))

(defn- other-account
  "Returns the name of the account on the other side of the transaction, or
  \"multiple\" when the transaction has more than two items"
  [item {:transaction/keys [items]} account-names]
  (let [others (remove #(util/id= item %) items)]
    (if (< 1 (count others))
      "multiple"
      (some-> others first :transaction-item/account :id account-names))))

(defn- format-decimal
  "Formats the decimal with at least two decimal places, and as many more as
  it has significant digits, so it reads the same however it was stored"
  [d]
  (let [stripped (.stripTrailingZeros d)]
    (.toPlainString (if (< (.scale stripped) 2)
                      (.setScale stripped 2)
                      stripped))))

(defn- ->row
  [fields account {:keys [transactions account-names completed-recon-ids]}]
  (fn [{:transaction-item/keys [balance memo reconciliation transaction]
        :transaction/keys [transaction-date description]
        :as item}]
    (let [trx (transactions (:id transaction))
          values {:transaction-date (str transaction-date)
                  :description description
                  :amount (some-> (assoc item :transaction-item/account account)
                                  polarize-quantity
                                  format-decimal)
                  :balance (some-> balance format-decimal)
                  :memo (or memo (:transaction/memo trx))
                  :other-account (other-account item trx account-names)
                  :reconciled (if (completed-recon-ids (:id reconciliation))
                                "yes"
                                "no")}]
      (map (comp values first) fields))))

(defn- ->csv
  [rows]
  (with-out-str
    (csv/write-csv *out* rows)))

(defn- file-name
  [{:account/keys [name]}]
  (str (-> name
           string/lower-case
           (string/replace #"[^a-z0-9]+" "-")
           (string/replace #"^-|-$" ""))
       "-transactions.csv"))

(defn- export
  [{:keys [params] :as req}]
  (if-let [account (find-account req)]
    (let [items (drop-through-zero-balance (fetch-items account req)
                                           params)
          transactions (fetch-transactions items)
          fields (parse-fields params)
          rows (map (->row fields
                           account
                           {:transactions transactions
                            :account-names (fetch-account-names transactions)
                            :completed-recon-ids (fetch-completed-reconciliation-ids items)})
                    items)]
      (-> (->csv (cons (map second fields) rows))
          res/response
          (res/content-type "text/csv")
          (res/header "Content-Disposition"
                      (format "attachment; filename=\"%s\""
                              (file-name account)))))
    api/not-found))

(def routes
  ["accounts/:account-id/transaction-items/export" {:get {:handler export}}])
