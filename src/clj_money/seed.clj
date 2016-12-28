(ns clj-money.seed
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-time.periodic :refer [periodic-seq]]
            [clj-time.format :refer [parse-local-date]]
            [clj-money.inflection :refer [keywordize]]
            [clj-money.serialization :refer [realize]]
            [clj-money.models.helpers :refer [with-storage
                                              with-transacted-storage]]
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
                 (fnil #(concat (accounts/select-by-entity-id storage-spec
                                                              (:id entity))
                                %)
                       [])))))

(defn seed
  [user entity identifier]
  (with-transacted-storage [s (env :db)]
    (pprint
      (->> identifier
           (format "resources/seeds/%s.edn")
           slurp
           read-string
           (append-user s user)
           (append-entity s entity)
           (append-accounts s entity)
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
  [email entity-name start-date]
  (with-transacted-storage [s (env :db)]
    (let [user (users/find-by-email s email)
          entity (entities/find-by-name s user entity-name)
          start-date (parse-local-date start-date)
          all-accounts (accounts/select-by-entity-id s (:id entity))
          accounts (->> ["Salary"
                         "FIT"
                         "Social Security"
                         "Medicare"
                         "Checking"]
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
                  (map #(generate-salary-transaction s entity % accounts)))))))
