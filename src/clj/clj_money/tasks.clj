(ns clj-money.tasks
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.java.shell :refer [sh]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [stowaway.implicit :refer [with-transacted-storage]]
            [clj-money.accounts :refer [nest unnest]]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]))

(defn- usage?
  [{:keys [options]}]
  (some #(% options) [:help? :usage?]))

(defn- print-usage
  [parsed-opts opts-spec]
  (println "USAGE:")
  (println (:usage opts-spec))
  (println "OPTIONS:")
  (println (:summary parsed-opts)))

(defn- print-error
  [parsed-opts opts-spec]
  (println "ERROR:")
  (doseq [e (:errors parsed-opts)]
    (println (str "  " e)))
  (print-usage parsed-opts opts-spec))

(defmacro with-options
  [args opts-spec bindings & body]
  `(let [parsed# (parse-opts ~args (:options ~opts-spec))
         f# (fn* [~(first bindings)] ~@body)]
     (cond
       (usage? parsed#)  (print-usage parsed# ~opts-spec)
       (:errors parsed#) (print-error parsed# ~opts-spec)
       :else             (f# (:options parsed#)))))

(def ^:private default-opts
  [["-h" "--help" "Show usage instructions"
    :id :help?]
   [nil "--usage" "Show usage instructions"
    :id :usage?]])

(def ^:private recalc-options
  {:usage "lein recalc <options>"
   :options (concat
              [["-a" "--account ACCOUNT_NAME" "The name of the account to be updated"
                :id :account-path
                :default :all
                :parse-fn #(string/split % #"/")]
               ["-u" "--user USER" "Identifies the user for which an entity is to be recalculated"
                :id :user-email
                :missing "A user must be specified"]
               ["-e" "--entity ENTITY_NAME" "The name of the entity to be recalculated"
                :id :entity-name
                :missing "An entity must be specified"]]
              default-opts)})

(defn- assoc-parent
  [criteria account-path entity]
  (if account-path
    (let [parent (->> account-path
                      butlast
                      (reduce (fn [p n]
                                (accounts/find-by {:entity-id (:id entity)
                                                   :name n
                                                   :parent-id (:id p)}))
                              nil))]
      (assoc criteria
             :parent-id (:id parent)
             :name (last account-path)))
    criteria))

(defn- account-criteria
  [account-path entity]
  (cond-> {:entity-id (:id entity)}
    (not= :all account-path)
    (assoc-parent account-path entity)))

(defn recalc
  [& args]
  (with-options args recalc-options [opts]
    (with-transacted-storage (env :db)
      (let [user (users/find-by-email (:user-email opts))
            _ (assert user (format "Unable to find user with email address \"%s\"." (:user-email opts)))
            entity (entities/find-by {:user-id (:id user)
                                      :name (:entity-name opts)})
            _ (assert entity (format "Unable to find entity with name \"%s\"." (:entity-name opts)))
            accounts (accounts/search (account-criteria (:account-name opts) entity))]
        (if (seq accounts)
          (do
            (doseq [account accounts]
              (println (format "Processing account \"%s\"..." (:name account)))
              (transactions/recalculate-account
                (:id account)
                (or (:earliest-transaction-date account)
                    (t/local-date 2006 1 1))
                {:force true})
              (println ""))
            (println "Done."))
          (println "No accounts found."))))))

(def ^:private update-commodity-price-ranges-options
  {:usage "lein update-commodity-price-ranges <options"
   :options (concat [["-c" "--commodity SYMBOL" "The symbol of the commodity to be updated"
                      :id :commodity-symbol
                      :default :all]
                     ["-u" "--user USER_EMAIL" "The email address for the user that owns the entity to be updated"
                      :id :user-email
                      :missing "A user email must be specified."]
                     ["-e" "--entity ENTITY_NAME" "The name of the entity for which one or more commodities are to be updated"
                      :id :entity-name
                      :missing "An entity name must be specified."]]
                    default-opts)})

(defn update-commodity-price-ranges
  [& args]
  (with-options args update-commodity-price-ranges-options [opts]
    (with-transacted-storage (env :db)
      (let [user (users/find-by-email (:user-email opts))
            _ (assert user (format "Unable to find user with email address \"%s\"." (:user-email opts)))
            entity (entities/find-by {:user-id (:id user)
                                      :name (:entity-name opts)})
            _ (assert entity (format "Unable to find entity named \"%s\"." (:entity-name opts)))
            commodities (commodities/search (cond-> {:entity-id (:id entity)}
                                              (not= :all (:commodity-symbol opts))
                                              (assoc :symbol (:commodity-symbol opts))))]
        (if (seq commodities)
          (do
            (doseq [commodity commodities]
              (println (format "Processing commodity \"%s\"..." (:symbol commodity)))
              (prices/rebound-commodity commodity)
              (println ""))
            (println "Done."))
          (println "No commodities found."))))))

(def ^:private migrate-account-cli-options
  {:usage "lein migrate-account <options"
   :options (concat [["-f" "--from-account FROM_ACCOUNT" "The name of the source account"
                      :id :from-account
                      :missing "The name of the \"from\" account must be specified."]
                     ["-t" "--to-account TO_ACCOUNT" "The name of the target account"
                      :id :to-account
                      :missing "The name of the \"to\" account must be specified."]
                     ["-u" "--user USER_EMAIL" "The email address of the user that owns the entity to be updated"
                      :id :user-email
                      :missing "The user email must be specified"]
                     ["-e" "--entity ENTITY_NAME" "The name of the entity to be updated"
                      :id :entity-name
                      :missing "The entity name must be specified"]]
                    default-opts)})

(defn migrate-account
  [& args]
  (with-options args migrate-account-cli-options [opts]
    (let [user (users/find-by {:email (:user-email opts)})
          _ (assert user (format "Unable to find user with email address \"%s\"." (:user-email opts)))
          entity (entities/find-by {:user-id (:id user)
                                    :name (:entity-name opts)})
          _ (assert entity (format "Unable to find an entity named \"%s\"." (:entity-name opts)))
          from-account (accounts/find-by {:entity-id (:id entity)
                                          :name (:from-account opts)})
          _ (assert from-account (format "Unable to find an account named \"%s\"." (:from-account opts)))
          to-account (accounts/find-by {:entity-id (:id entity)
                                        :name (:to-account opts)})]
      (assert to-account (format "Unable to find an account named \"%s\"." (:to-account opts)))
      (transactions/migrate-account from-account to-account))))

(def ^:private export-user-tags-cli-options
  {:usage "lein export-user-tags <options>"
   :options (concat [["-u" "--user USER_EMAIL" "The email address of the user that owns the entity"
                      :id :user-email
                      :missing "A user email must be specified."]
                     ["-e" "--entity ENTITY_NAME" "The name of the entity from which user tags are to be exported"
                      :id :entity-name
                      :missing "An entity name must be specified."]
                     ["-o" "--output-file OUTPUT_FILE" "The path and filename where the user tags are to be written"
                      :id :output-file
                      :missing "An output file must be specified."]]
                    default-opts)})

(defn export-user-tags
  [& args]
  (with-options args export-user-tags-cli-options [opts]
    (let [user (users/find-by {:email (:user-email opts)})
          _ (assert user (format "Unable to find a user with email address \"%s\"." (:user-email opts)))
          entity (entities/find-by {:user-id (:id user)
                                    :name (:entity-name opts)})
          _ (assert entity (format "Unable to find an entity named \"%s\"." (:entity-name opts)))]
      (spit (:output-file opts)
            (->> (accounts/search {:entity-id (:id entity)})
                 nest
                 unnest
                 (filter #(seq (:user-tags %)))
                 (map #(select-keys % [:path :user-tags]))
                 prn-str)))))

(def ^:private import-user-tags-cli-options
  {:usage "lein import-user-tags <options>"
   :options (concat [["-u" "--user USER_EMAIL" "The email address of the user that owns the entity"
                      :id :user-email
                      :missing "The user email must be specified."]
                     ["-e" "--entity ENTITY_NAME" "The name of the entity to be updated"
                      :id :entity-name
                      :missing "The entity name must be specified"]
                     ["-i" "--input-file INPUT_FILE" "The file containing the user tag data"
                      :id :input-file
                      :missing "The input file must be specified"]]
                    default-opts)})

(defn import-user-tags
  [& args]
  (with-options args import-user-tags-cli-options [opts]
    (let [user (users/find-by {:email (:user-email opts)})
          _ (assert user (format "Unable to find a user with email address \"%s\"." (:user-email opts)))
          entity (entities/find-by {:user-id (:id user)
                                    :name (:entity-name opts)})
          _ (assert entity (format "Unable to find an entity named \"%s\"." (:entity-name opts)))
          accounts (->> (accounts/search {:entity-id (:id entity)
                                                     :type "expense"})
                        nest
                        unnest
                        (group-by :path))
          tags (edn/read-string (slurp (:input-file opts)))]

      (doseq [{:keys [path user-tags]} tags
              account (get-in accounts [path])]
        (accounts/update (assoc account :user-tags user-tags))))))

(defn compile-sass []
  (println (:out (sh "resources/compile-sass.sh"))))
