(ns clj-money.tasks
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.java.shell :refer [sh]]
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
                  :or {validate (constantly nil)}}]
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

(defn- account-criteria
  [options entity]
  (let [parent (->> (:account options)
                    butlast
                    (reduce (fn [p n]
                              (accounts/find-by {:entity-id (:id entity)
                                                 :name n
                                                 :parent-id (:id p)}))
                            nil))]
    (cond-> {:entity-id (:id entity)}
      parent
      (assoc :parent-id (:id parent))

      (not= :all (:account options))
      (assoc :name (last (:account options))))))

(def ^:private recalc-options
  [["-a" "--account" "Account name"
    :default :all
    :parse-fn #(string/split % #"/")
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
              accounts (accounts/search (account-criteria options entity))]
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
    :id :help?]])

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
    (let [user (users/find-by {:email (:user opts)})
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

(def ^:private export-user-tags-cli-options
  [["-u" "--user" "Username (email address)"
    :required "Identifies the user account from which user tags are to be exported"]
   ["-e" "--entity" "Entity name"
    :required "Identifies the entity from which user tags are to be exported"]
   ["-o" "--output-file" "Output File"
    :required "The location where the tags are to be written"]
   ["-h" "--help"
    :id :help?]])

(defn export-user-tags
  [& args]
  (when-let [opts (read-args
                    args
                    export-user-tags-cli-options
                    {:title "EXPORT USER TAGS"})]
    (let [user (users/find-by {:email (:user opts)})
          entity (entities/find-by {:user-id (:id user)
                                    :name (:entity opts)})
          _ (assert entity "Entity not found")]
      (spit (:output-file opts)
            (->> (accounts/search {:entity-id (:id entity)})
                 (filter #(seq (:user-tags %)))
                 (map #(select-keys % [:name :user-tags]))
                 prn-str)))))

(def ^:private import-user-tags-cli-options
  [["-u" "--user" "Username (email address)"
    :required "Identifies the user account into which user tags are to be imported"]
   ["-e" "--entity" "Entity name"
    :required "Identifies the entity into which user tags are to be imported"]
   ["-i" "--input-file" "Input File"
    :required "The location from which the tags are to be read"]
   ["-h" "--help"
    :id :help?]])

(defn import-user-tags
  [& args]
  (when-let [opts (read-args
                   args
                   import-user-tags-cli-options
                   {:title "IMPORT USER TAGS"})]
    (let [user (users/find-by {:email (:user opts)})
          _ (assert user "User not found")
          entity (entities/find-by {:user-id (:id user)
                                    :name (:entity opts)})
          _ (assert entity "Entity not found")
          accounts (group-by :name (accounts/search {:entity-id (:id entity)}))
          tags (edn/read-string (slurp (:input-file opts)))]

      (doseq [{:keys [name user-tags]} tags
              account (get-in accounts [name])]
        (accounts/update (assoc account :user-tags user-tags))))))

(defn compile-sass []
  (println (:out (sh "resources/compile-sass.sh"))))
