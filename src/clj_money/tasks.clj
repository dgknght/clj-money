(ns clj-money.tasks
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.edn :as edn]
            [clj-money.models.schema :as schema]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.models :as models]
            [clj-money.accounts :refer [nest unnest]]
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
    (let [user (models/find-by {:user/email (:user-email opts)})
          _ (assert user (format "Unable to find user with email address \"%s\"." (:user-email opts)))
          entity (models/find-by {:entity/user user
                                  :entity/name (:entity-name opts)})
          _ (assert entity (format "Unable to find an entity named \"%s\"." (:entity-name opts)))
          from-account (models/find-by #:account{:entity entity
                                                 :name (:from-account opts)})
          _ (assert from-account (format "Unable to find an account named \"%s\"." (:from-account opts)))
          to-account (models/find-by #:account{:entity entity
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
    (let [user (models/find-by {:user/email (:user-email opts)})
          _ (assert user (format "Unable to find a user with email address \"%s\"." (:user-email opts)))
          entity (models/find-by #:entity{:user user
                                          :name (:entity-name opts)})
          _ (assert entity (format "Unable to find an entity named \"%s\"." (:entity-name opts)))]
      (spit (:output-file opts)
            (->> (models/select {:account/entity entity})
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
    (let [user (models/find-by {:user/email (:user-email opts)})
          _ (assert user (format "Unable to find a user with email address \"%s\"." (:user-email opts)))
          entity (models/find-by #:entity{:user user
                                          :name (:entity-name opts)})
          _ (assert entity (format "Unable to find an entity named \"%s\"." (:entity-name opts)))
          accounts (->> (models/select #:account{:entity entity
                                                 :type :expense})
                        nest
                        unnest
                        (group-by :path))
          tags (edn/read-string (slurp (:input-file opts)))]

      (doseq [{:keys [path user-tags]} tags
              account (get-in accounts [path])]
        (models/put (assoc account :account/user-tags user-tags))))))

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn er-diagram [& _args]
  (let [lines (concat ["erDiagram"]
                      (mapcat (fn [{:keys [id fields]}]
                                (concat [(str "  " (name id) " {")]
                                (map (fn [{:keys [id type]}]
                                         (str "    " (name type) " " (name id)))
                                     fields)
                                ["  }"]))
                              schema/models)
                      (mapcat (fn [{:keys [refs id]}]
                                (map (fn [ref]
                                       (str "  " (name ref) " ||--|{ " (name id) " : \"has many\""))
                                     refs))
                              schema/models))]
    (doseq [l lines] (println l))))
