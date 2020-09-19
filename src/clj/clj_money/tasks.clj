(ns clj-money.tasks
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [stowaway.implicit :refer [with-transacted-storage]]
            [clj-money.util :refer [presence]]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]))

(defn- read-args
  [args cli-opts {:keys [validate
                         title]
                  :or {validate (constantly true)}}]
  (let [{:keys [options
                 summary
                 errors]} (parse-opts args cli-opts)
        validation-error (when-not (:help? options)
                           (validate options))
        print-fn (fn []
                   (when title (println title))
                   (when validation-error (println validation-error))
                   (println summary))]
    (cond
      (:help? options) (print-fn)
      (seq errors)     (print-fn)
      validation-error (print-fn)
      :else            options)))

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
      (with-transacted-storage (env :db)
        (let [user (users/find-by-email (:user options))
              entity (entities/find-by {:user-id (:id user)
                                        :name (:entity options)})
              accounts (accounts/search (cond-> {:entity-id (:id entity)}
                                          (not= :all (:account options))
                                          (assoc :name (:account options))))]
          (doseq [account accounts]
            (println (format "Processing account \"%s\"..." (:name account)))
            (transactions/recalculate-account
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
      (with-transacted-storage (env :db)
        (let [user (users/find-by-email (:user options))
              entity (entities/find-by {:user-id (:id user)
                                        :name (:entity options)})
              commodities (commodities/search (cond-> {:entity-id (:id entity)}
                                                (not= :all (:commodity options))
                                                (assoc :symbol (:commodity options))))]
          (doseq [commodity commodities]
            (println (format "Processing commodity \"%s\"..." (:symbol commodity)))
            (prices/rebound-commodity commodity)
            (println ""))
          (println "Done."))))))

(def ^:private migrate-account-cli-options
  [["-f" "--from-account" "From Account name"
    :required "The name of the account that is the source of transactions to be migrated"]
   ["-t" "--to-account" "To Account name"
    :required "The name of the account to which the transactions are to be migrated"]
   ["-u" "--user" "Username (email address)"
    :required "Identifies the user account for which accounts are to be recalculated"]
   ["-e" "--entity" "Entity name"
    :required "Identifies the entity for which accounts are to be recalculated"]
   ["-h" "--help"
    :id :help? ]])

(defn- validate-migrate-account-options
  [options]
  (when-let [missing (->> [:user :entity :to-account :from-account]
                          (remove #(contains? options %))
                          (map name)
                          (string/join ", ")
                          presence)]
    (str "Missing attributes: " missing)))

(defn migrate-account
  [& args]
  (when-let [opts (read-args
               args
               migrate-account-cli-options
               {:title "MIGRATE ACCOUNT"
                :validate validate-migrate-account-options})]
    (let [user (users/find {:email (:user opts)})
          entity (entities/find-by {:user-id (:id user)
                                    :name (:entity opts)})
          _ (assert entity "Entity not found")
          from-account (accounts/find-by {:entity-id (:id entity)
                                          :name (:from-account opts)})
          _ (assert from-account "\"From\" account not found")
          to-account (accounts/find-by {:entity-id (:id entity)
                                        :name (:to-account opts)})]
      (assert to-account "\"To\" account not found")
      (transactions/migrate-account from-account to-account))))
