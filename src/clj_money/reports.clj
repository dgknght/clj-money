(ns clj-money.reports
  "Functions shared by the reports, each of which lives in a child namespace
  (e.g. clj-money.reports.balance-sheet)."
  (:require [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.inflection :refer [humanize]]
            [clj-money.entities :as entities]
            [clj-money.util :as util]
            [clj-money.accounts :as accounts :refer [system-tagged?
                                                     valuate]]
            [clj-money.entities.transactions :as transactions]
            [clj-money.entities.prices :as prices]))

(defn header?
  [{:report/keys [style]}]
  (= :header style))

(defn sum
  [attr col]
  (->> col
       (map attr)
       (reduce + 0M)))

(defn- fetch-account-value
  [since as-of]
  (if since
    (fn [a]
      (transactions/balance-delta a since as-of))
    (fn [a]
      (transactions/balance-as-of a as-of))))

(defn- balance-data
  [{:keys [accounts since as-of]}]
  (->> accounts
       (map (juxt :id (fetch-account-value since as-of)))
       (into {})))

(defn- valuation-data
  [{:keys [entity accounts as-of] :as options}]
  (let [balances (balance-data options)
        trading-account-ids (->> accounts
                                 (filter (system-tagged? :trading))
                                 (map :id))
        [earliest-date] (:entity/transaction-date-range entity)
        lots (when (seq trading-account-ids)
               (group-by (juxt (comp :id :lot/account)
                               (comp :id :lot/commodity))
                         (entities/select {:lot/account [:in trading-account-ids]
                                           :lot/purchase-date [:between
                                                               earliest-date
                                                               as-of]})))
        lot-items (when (seq lots)
                    (group-by (comp :id :lot-item/lot)
                              (entities/select
                                (util/entity-type
                                  {:lot-item/lot [:in (->> (vals lots)
                                                           (mapcat identity)
                                                           (mapv :id))]
                                   :transaction/transaction-date [:between
                                                                  earliest-date
                                                                  as-of]}
                                  :lot-item))))
        prices (atom {})]
    (reify accounts/ValuationData
      (fetch-entity [_ _account] entity)
      (fetch-balance [_ {:keys [id]}] (balances id))
      (fetch-lots [_ {:account/keys [parent commodity]}]
        (lots (mapv :id [parent commodity])))
      (fetch-lot-items [_ {:keys [id]}]
        (lot-items id))
      (fetch-price [_ {:keys [id] :as commodity}]
        (if-let [cached (get-in @prices [id])]
          cached
          (let [price (:price/value (prices/most-recent commodity as-of))]
            (when-not price
              (throw (ex-info "No price found for commodity" {:commodity commodity})))
            (swap! prices assoc id price)
            price))))))

(defn valuate-accounts
  "Given a sequence of accounts, assess their value based on the given date or
  range of dates.

  For accounts tracking the default commodity for the entity, the value comes
  from the balance of the transaction items that bound the date range.

  For accounts that track other commodities, the value comes from the quantity
  of that commodity held in the account and the most recent price based on the
  specified date."
  [opts accounts]
  (if (-> opts :entity :entity/transaction-date-range)
    (let [data (valuation-data
                 (-> opts
                     (assoc :accounts accounts)
                     (update-in [:as-of] (fnil identity (t/local-date)))))]
      (valuate data accounts))
    accounts))

(declare summarize-accounts)
(defn- summarize-account
  [account depth]
  (let [children (->> (:account/children account)
                      (summarize-accounts (inc depth))
                      (sort-by :account/name))]
    (cons (-> account
              (select-keys [:id
                            :account/name
                            :account/total-value
                            :account/type])
              (rename-keys {:account/name :report/caption
                            :account/total-value :report/value
                            :account/type :report/type})
              (assoc :report/style :data
                     :report/depth depth))
          children)))

(defn- summarize-accounts
  ([accounts] (summarize-accounts 0 accounts))
  ([depth accounts]
   (mapcat #(summarize-account % depth) accounts)))

(defn summarize-account-type
  "Given a map with an account type and the nested accounts of that type,
  returns a header record for the type followed by a record for each account"
  [{:keys [type accounts]}]
  (let [records (summarize-accounts accounts)]
    (cons {:report/caption (humanize type)
           :report/type type
           :report/style :header
           :report/value (->> records
                              (filter #(-> % :report/depth zero?))
                              (map :report/value)
                              (reduce + 0M))}
          (vec records))))

(defn map-record-headers
  "Returns a map of account type to the total value from the header records"
  [records]
  (->> records
       (filter header?)
       (map (juxt :report/type :report/value))
       (into {})))
