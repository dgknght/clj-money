(ns clj-money.views.transactions.overdraw
  "Warns when a transaction being entered would leave an asset account with a
  negative balance as of the transaction's date.

  The balance as of that date is worked out from the account's cached
  current balance, less the items that come after the date and, when an
  existing transaction is being edited, less its effect as it was saved.
  It only warns, since overdrafts can be legitimate and the cached balance
  may lag behind changes made elsewhere."
  (:require [clojure.string :as string]
            [cljs-time.core :as t]
            [reagent.core :as r]
            [dgknght.app-lib.web :refer [format-decimal]]
            [dgknght.app-lib.decimal :as decimal]
            [clj-money.util :refer [id=]]
            [clj-money.dates :refer [format-local-date]]
            [clj-money.state :refer [accounts-by-id]]
            [clj-money.transactions :as trxs :refer [accountified?
                                                     unentryfy
                                                     balance-changes]]
            [clj-money.api.transactions :as transactions]
            [clj-money.api.transaction-items :as trx-items]))

(defn- with-cached-account
  [item]
  (update-in item
             [:transaction-item/account]
             #(some-> % :id (@accounts-by-id))))

(defn- changes
  "Returns the balance changes the transaction makes, whether it's in the
  simplified form or the full one"
  [trx]
  (->> (unentryfy (if (accountified? trx)
                    (trxs/expand trx)
                    trx))
       :transaction/items
       (map with-cached-account)
       balance-changes))

(defn- spent-accounts
  "Returns the asset accounts whose balances the transaction reduces, as
  [account change] tuples"
  [trx]
  (->> (changes trx)
       (keep (fn [[id change]]
               (let [account (@accounts-by-id id)]
                 (when (and (= :asset (:account/type account))
                            (decimal/< change (decimal/zero)))
                   [account change]))))))

(defn- later-items-key
  [{:keys [id]} date]
  [:later id date])

(defn- original-key
  [{:keys [id]}]
  [:original id])

(defn- needs-later-items?
  "Items after the date need only be fetched when the account has some"
  [{[_ end] :account/transaction-date-range} date]
  (and end (t/after? end date)))

(defn- fetch-later-items
  [fetched {[_ end] :account/transaction-date-range :as account} date]
  (let [k (later-items-key account date)]
    (swap! fetched assoc k :pending)
    (trx-items/select {:transaction-item/account account
                       :transaction/transaction-date [:between
                                                      (t/plus date (t/days 1))
                                                      end]}
                      :on-success #(swap! fetched assoc k %)
                      :on-error #(swap! fetched assoc k :failed))))

(defn- fetch-original
  [fetched trx]
  (let [k (original-key trx)]
    (swap! fetched assoc k :pending)
    (transactions/get trx
                      :on-success #(swap! fetched
                                          assoc
                                          k
                                          (->> (:transaction/items %)
                                               (map with-cached-account)
                                               balance-changes))
                      :on-error #(swap! fetched assoc k :failed))))

(defn- fetch-missing
  "Fetches whatever isn't yet known that's needed to work out the balances
  of the accounts the transaction spends from as of its date"
  [transaction fetched]
  (let [{:transaction/keys [transaction-date] :as trx} @transaction
        spent (when transaction-date (spent-accounts trx))
        known @fetched]
    (when (seq spent)
      (when (and (:id trx)
                 (not (contains? known (original-key trx))))
        (fetch-original fetched trx))
      (doseq [[account] spent]
        (when (and (needs-later-items? account transaction-date)
                   (not (contains? known (later-items-key account transaction-date))))
          (fetch-later-items fetched account transaction-date))))))

(defn- later-change
  "Returns the net change made to the account by the items after the date,
  other than those of the transaction being edited, or nil while they're
  still being fetched"
  [known {:as account :keys [id]} date trx]
  (if (needs-later-items? account date)
    (let [items (known (later-items-key account date))]
      (when (sequential? items)
        (->> items
             (remove #(and (:id trx)
                           (id= trx (:transaction-item/transaction %))))
             (map with-cached-account)
             balance-changes
             (#(get % id (decimal/zero))))))
    (decimal/zero)))

(defn- original-change
  "Returns the change the transaction being edited made to the account as
  it was saved, or nil while that's still being fetched"
  [known {:keys [id]} trx]
  (if (:id trx)
    (let [original (known (original-key trx))]
      (when (map? original)
        (get original id (decimal/zero))))
    (decimal/zero)))

(defn- overdraws
  "Returns the accounts the transaction would overdraw as of its date,
  each with the balance it would leave, as [account balance] tuples"
  [{:transaction/keys [transaction-date] :as trx} known]
  (when transaction-date
    (keep (fn [[account change]]
            (let [later (later-change known account transaction-date trx)
                  original (original-change known account trx)]
              (when (and later original)
                (let [balance (-> (or (:account/quantity account) (decimal/zero))
                                  (decimal/- later)
                                  (decimal/- original)
                                  (decimal/+ change))]
                  (when (decimal/< balance (decimal/zero))
                    [account balance])))))
          (spent-accounts trx))))

(defn overdraw-warning
  "Renders a warning when the transaction in the given atom (or cursor)
  would overdraw an asset account as of its date"
  [transaction]
  (let [fetched (r/atom {})
        tracker (r/track! fetch-missing transaction fetched)]
    (r/create-class
      {:display-name "overdraw-warning"
       :component-will-unmount #(r/dispose! tracker)
       :reagent-render
       (fn [transaction]
         (let [{:transaction/keys [transaction-date] :as trx} @transaction]
           (when-let [overdrawn (seq (overdraws trx @fetched))]
             [:div#overdraw-warning.alert.alert-warning
              (str "This transaction would overdraw "
                   (string/join ", "
                                (map (fn [[account balance]]
                                       (str (:account/name account)
                                            " (balance "
                                            (format-decimal balance)
                                            ")"))
                                     overdrawn))
                   " as of "
                   (format-local-date transaction-date)
                   ".")])))})))
