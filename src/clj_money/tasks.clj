(ns clj-money.tasks
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [clj-money.entities.schema :as schema]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.entities :as entities]
            [clj-money.accounts :refer [nest unnest]]
            [clj-money.entities.transactions :as transactions]))

(def ^:private usage?
  (comp :help? :options))

(defn- print-usage
  [{:keys [summary]} {:keys [usage description]}]
  (println description)
  (println "")
  (println "USAGE:")
  (println usage)
  (println "")
  (println "OPTIONS:")
  (println summary))

(defn- print-error
  [parsed-opts opts-spec]
  (println "ERROR:")
  (doseq [e (:errors parsed-opts)]
    (println (str "  " e)))
  (print-usage parsed-opts opts-spec))

(defmacro with-options
  [[arg-sym options] & body]
  `(let [opts# ~options
         spec# (:options opts#)
         parsed# (parse-opts (:args opts#)
                             spec#)
         f# (fn* [~arg-sym] ~@body)]
     (cond
       (usage? parsed#)  (print-usage parsed# opts#)
       (:errors parsed#) (print-error parsed# opts#)
       :else             (f# parsed#))))

(def ^:private default-opts
  [["-h" "--help" "Show usage instructions"
    :id :help?]])

(def ^:private migrate-account-cli-options
  {:usage "lein migrate-account <options>"
   :description "Move transactions from one account to another"
   :options (conj default-opts
                  ["-f" "--from-account FROM_ACCOUNT" "The name of the source account"
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
                   :missing "The entity name must be specified"])})

(defn migrate-account
  [& args]
  (with-options [parsed (assoc migrate-account-cli-options
                               :args args)]
    (let [{{:keys [user-email
                   entity-name
                   from-account
                   to-account]} :options} parsed
          user (entities/find-by {:user/email user-email})
          _ (assert user
                    (format "Unable to find user with email address \"%s\"."
                            user-email))
          entity (entities/find-by {:entity/user user
                                    :entity/name entity-name})
          _ (assert entity
                    (format "Unable to find an entity named \"%s\"."
                            entity-name))
          from-account (entities/find-by #:account{:entity entity
                                                   :name from-account})
          _ (assert from-account
                    (format "Unable to find an account named \"%s\"."
                            from-account))
          to-account (entities/find-by #:account{:entity entity
                                                 :name to-account})]
      (assert to-account
              (format "Unable to find an account named \"%s\"."
                      to-account))
      (transactions/migrate-account from-account to-account))))

(def ^:priviate re-index-cli-options
  {:usage "lein re-index <options>"
   :description "Recalculate :account-item/index and :account-item/balance for the specified account or for all accounts in the specified entity"
   :options (conj default-opts
                  ["-a" "--account ACCOUNT" "The name of the account to re-index"
                   :id :account-name]
                  ["-u" "--user USER_EMAIL" "The email address of the user that owns the entity to be updated"
                   :id :user-email
                   :missing "The user email must be specified"]
                  ["-e" "--entity ENTITY_NAME" "The name of the entity to be updated"
                   :id :entity-name
                   :missing "The entity name must be specified"])})

(defn re-index
  [& args]
  (with-options [parsed (assoc re-index-cli-options
                               :args args)]
    (let [{:keys [user-email
                  entity-name
                  account-name]} (:options parsed)
          user (entities/find-by {:user/email user-email})
          _ (assert user
                    (format "Unable to find user with email address \"%s\"."
                            user-email))
          entity (entities/find-by {:entity/user user
                                    :entity/name entity-name})
          _ (assert entity
                    (format "Unable to find an entity named \"%s\"."
                            entity-name))
          accounts (entities/select (cond-> {:account/entity entity}
                                      account-name
                                      (assoc :account/name account-name)))]
      (doseq [account accounts]
        (println "Reindexing" (:account/name account) "...")
        (transactions/propagate-account-from-start entity account))
      (println "Done.")))
  (shutdown-agents))

(def ^:private export-user-tags-cli-options
  {:usage "lein export-user-tags <options>"
   :description "Write the user-defined tags for all accounts in an entity to the a file."
   :options (conj default-opts
                  ["-u" "--user USER_EMAIL" "The email address of the user that owns the entity"
                   :id :user-email
                   :missing "A user email must be specified."]
                  ["-e" "--entity ENTITY_NAME" "The name of the entity from which user tags are to be exported"
                   :id :entity-name
                   :missing "An entity name must be specified."]
                  ["-o" "--output-file OUTPUT_FILE" "The path and filename where the user tags are to be written"
                   :id :output-file
                   :missing "An output file must be specified."])})

(defn export-user-tags
  [& args]
  (with-options [parsed (assoc export-user-tags-cli-options
                               :args args)]
    (let [{:keys [user-email
                  entity-name
                  output-file]} (:options parsed)
          user (entities/find-by {:user/email user-email})
          _ (assert user
                    (format "Unable to find a user with email address \"%s\"."
                            user-email))
          entity (entities/find-by #:entity{:user user
                                            :name entity-name})
          _ (assert entity
                    (format "Unable to find an entity named \"%s\"."
                            entity-name))]
      (spit output-file
            (->> (entities/select {:account/entity entity})
                 nest
                 unnest
                 (filter #(seq (:user-tags %)))
                 (map #(select-keys % [:path :user-tags]))
                 prn-str)))))

(def ^:private import-user-tags-cli-options
  {:usage "lein import-user-tags <options>"
   :description "Import the user-defined tags for an entity from a file."
   :options (conj default-opts
                  ["-u" "--user USER_EMAIL" "The email address of the user that owns the entity"
                   :id :user-email
                   :missing "The user email must be specified."]
                  ["-e" "--entity ENTITY_NAME" "The name of the entity to be updated"
                   :id :entity-name
                   :missing "The entity name must be specified"]
                  ["-i" "--input-file INPUT_FILE" "The file containing the user tag data"
                   :id :input-file
                   :missing "The input file must be specified"])})

(defn import-user-tags
  [& args]
  (with-options [parsed (assoc import-user-tags-cli-options
                               :args args)]
    (let [{:keys [entity-name
                  user-email
                  input-file]} (:options parsed)
          user (entities/find-by {:user/email user-email})
          _ (assert user
                    (format "Unable to find a user with email address \"%s\"."
                            user-email))
          entity (entities/find-by #:entity{:user user
                                            :name entity-name})
          _ (assert entity
                    (format "Unable to find an entity named \"%s\"."
                            entity-name))
          accounts (->> (entities/select #:account{:entity entity
                                                   :type :expense})
                        nest
                        unnest
                        (group-by :path))
          tags (edn/read-string (slurp input-file))]

      (doseq [{:keys [path user-tags]} tags
              account (get-in accounts [path])]
        (entities/put (assoc account :account/user-tags user-tags))))))

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn er-diagram [& _args]
  (let [lines (concat ["erDiagram"]
                      (mapcat (fn [{:keys [id fields]}]
                                (concat [(str "  " (name id) " {")]
                                (map (fn [{:keys [id type]}]
                                         (str "    " (name type) " " (name id)))
                                     fields)
                                ["  }"]))
                              schema/entities)
                      (mapcat (fn [{:keys [refs id]}]
                                (map (fn [ref]
                                       (str "  " (name ref) " ||--|{ " (name id) " : \"has many\""))
                                     refs))
                              schema/entities))]
    (doseq [l lines] (println l))))
