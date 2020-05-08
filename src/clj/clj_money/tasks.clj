(ns clj-money.tasks
  (:require [clojure.tools.cli :refer [parse-opts]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [stowaway.core :refer [with-transacted-storage]]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]))

(def ^:private recalc-options
  [["-a" "--account" "Account name"
    :default :all
    :required "The name of the account to be recalculated"]
   ["-u" "--user" "Username (email address)"
    :required "Identifies the user account for which accounts are to be recalculated"]
   ["-e" "--entity" "Entity name"
    :required "Identifies the entity for which accounts are to be recalculated"]])

(defn recalc
  [& args]
  (let [{:keys [options summary errors]} (parse-opts args recalc-options)]
    (cond
      (seq errors)
      (println errors)

      (not-every? #(% options) [:user :entity])
      (println summary)

      :else
      (with-transacted-storage [s (env :db)]
        (let [user (users/find-by-email s (:user options))
              entity (entities/find-by s {:user-id (:id user)
                                          :name (:entity options)})
              accounts (accounts/search s (cond-> {:entity-id (:id entity)}
                                            (not= :all (:account options))
                                            (assoc :name (:account options))))]
          (doseq [account accounts]
            (println (format "Processing account \"%s\"..." (:name account)))
            (transactions/recalculate-account
              s
              (:id account)
              (or (:earliest-transaction-date account)
                  (t/local-date 2006 1 1))
              {:force true})
            (println ""))
          (println "Done."))))))

(def ^:private update-commodity-price-ranges-options
  [["-c" "--commodity" "Commodity symbol"
    :default :all
    :required "The name of the account to be recalculated"]
   ["-u" "--user" "Username (email address)"
    :required "Identifies the user account for which accounts are to be recalculated"]
   ["-e" "--entity" "Entity name"
    :required "Identifies the entity for which accounts are to be recalculated"]])

(defn update-commodity-price-ranges [& args]
  (let [{:keys [options summary errors]} (parse-opts args update-commodity-price-ranges-options)]
    (cond
      (seq errors)
      (println errors)

      (not-every? #(% options) [:user :entity])
      (println summary)

      :else
      (with-transacted-storage [s (env :db)]
        (let [user (users/find-by-email s (:user options))
              entity (entities/find-by s {:user-id (:id user)
                                          :name (:entity options)})
              commodities (commodities/search s (cond-> {:entity-id (:id entity)}
                                                  (not= :all (:commodity options))
                                                  (assoc :symbol (:commodity options))))]
          (doseq [commodity commodities]
            (println (format "Processing commodity \"%s\"..." (:symbol commodity)))
            (prices/rebound-commodity
              s
              commodity)
            (println ""))
          (println "Done."))))))
