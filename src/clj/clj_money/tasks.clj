(ns clj-money.tasks
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.cli :refer [parse-opts]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-money.models.helpers :refer [with-transacted-storage]]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]))

(def ^:private recalc-options
  [["-a" "--account" "Account name"
    :required "An account must be specified"]
   ["-u" "--user" "Username (email address)"
    :required "A User must be specified"]
   ["-e" "--entity" "Entity name"
    :required "An entity must be specified"]])

(defn recalc
  [& args]
  (let [{:keys [options summary errors]} (parse-opts args recalc-options)]
    (cond
      (seq errors)
      (println errors)

      (empty? options)
      (println summary)

      :else
      (with-transacted-storage [s (env :db)]
        (let [user (users/find-by-email s (:user options))
              entity (entities/find-by-name s user (:entity options))
              account (accounts/find-by s {:entity-id (:id entity)
                                                   :name (:account options)})]
          (transactions/recalculate-account
            s
            (:id account)
            (or (:earliest-transaction-date account)
                (t/local-date 2015 1 1)))
          (println "Done."))))))
