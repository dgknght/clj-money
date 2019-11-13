(ns clj-money.seed
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-time.periodic :refer [periodic-seq]]
            [clj-time.format :refer [parse-local-date]]
            [clj-money.inflection :refer [keywordize]]
            [clj-money.serialization :refer [realize]]
            [clj-money.models.helpers :refer [with-transacted-storage]]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.users :as users])
  (:import (org.joda.time Months
                          Weeks
                          Days)))

(defn- append-entity
  [storage-spec entity context]
  (update-in context
             [:entities]
             (fnil #(conj % (entities/find-by-name storage-spec
                                                 (-> context :users first)
                                                 entity))
                   [])))

(defn- append-user
  [storage-spec user context]
  (update-in context
             [:users]
             (fnil #(conj % (users/find-by-email storage-spec user)) [])))

(defn- append-accounts
  [storage-spec entity-name context]
  (let [entity (->> (:entities context)
                    (filter #(= entity-name (:name %)))
                    first)]
    (if (-> context :accounts map?)
      context
      (update-in context
                 [:accounts]
                 (fnil #(concat (accounts/search storage-spec
                                                 {:entity-id (:id entity)})
                                %)
                       [])))))

(defn seed
  "Seeds an entity with a predefined set of accounts

  USAGE:
  lein seed john@doe.com Personal personal-accounts"
  [email entity-name seed-identifier]
  (with-transacted-storage [s (env :db)]
    (pprint
      (->> seed-identifier
           (format "resources/seeds/%s.edn")
           slurp
           read-string
           (append-user s email)
           (append-entity s entity-name)
           (append-accounts s entity-name)
           (realize s)))))

(defn- generate-salary-transaction
  [storage entity transaction-date {:keys [salary
                                           fit
                                           social-security
                                           medicare
                                           checking]}]
  (transactions/create storage
                       {:entity-id (:id entity)
                        :transaction-date transaction-date
                        :description "Paycheck"
                        :items [{:action :credit
                                 :account-id (:id salary)
                                 :amount 2000M}
                                {:action :debit
                                 :account-id (:id fit)
                                 :amount 400M}
                                {:action :debit
                                 :account-id (:id social-security)
                                 :amount 124M}
                                {:action :debit
                                 :account-id (:id medicare)
                                 :amount 29M}
                                {:action :debit
                                 :account-id (:id checking)
                                 :amount 1447M}]}))

(defn generate-transactions
  "Seeds an account with transactions

  USAGE
  lein generate-transactions john@doe.com Personal 2017-01-01"
  [email entity-name start-date]
  (with-transacted-storage [s (env :db)]
    (let [user (users/find-by-email s email)
          entity (entities/find-by-name s user entity-name)
          start-date (parse-local-date start-date)
          all-accounts (accounts/search s {:entity-id (:id entity)})
          accounts (->> ["Salary"
                         "FIT"
                         "Social Security"
                         "Medicare"
                         "Rent"
                         "Groceries"
                         "Checking"
                         "Credit card"]
                        (map (fn [account-name]
                               [(keywordize account-name)
                                (->> all-accounts
                                     (filter #(= account-name (:name %)))
                                     first)]))
                        (into {}))]

      (when (some nil? (vals accounts))
        (throw (RuntimeException. (str "At least one account could not be found. " accounts))))

      ; Salary
      (dorun (->> [start-date (t/plus start-date (Days/days 14))]
                  (map #(periodic-seq % Months/ONE))
                  (apply interleave)
                  (take 24)
                  (map #(generate-salary-transaction s entity % accounts))))

      ; Rent
      (dorun (->> (periodic-seq (t/plus start-date Days/ONE) Months/ONE)
                  (take 12)
                  (map #(transactions/create
                          s
                          {:entity-id (:id entity)
                           :transaction-date %
                           :description "Landlord"
                           :items [{:action :debit
                                    :account-id (-> accounts :rent :id)
                                    :amount 750M}
                                   {:action :credit
                                    :account-id (-> accounts :checking :id)
                                    :amount 750M}]}))))

      ; Groceries
      (dorun (->> (periodic-seq (t/plus start-date Days/TWO) Weeks/ONE)
                  (take 52)
                  (map #(transactions/create
                          s
                          {:entity-id (:id entity)
                           :transaction-date %
                           :description "Market Street"
                           :items [{:action :debit
                                    :account-id (-> accounts :groceries :id)
                                    :amount 89M}
                                   {:action :credit
                                    :account-id (-> accounts :credit-card :id)
                                    :amount 89M}]}))))

      ; Credit card
      (dorun (->> (periodic-seq (t/plus start-date (Days/days 10)) Months/ONE)
                  (take 12)
                  (map #(transactions/create
                          s
                          {:entity-id (:id entity)
                           :transaction-date %
                           :description "Discover Card"
                           :items [{:action :debit
                                    :account-id (-> accounts :credit-card :id)
                                    :amount 325M}
                                   {:action :credit
                                    :account-id (-> accounts :checking :id)
                                    :amount 325M}]})))))))
