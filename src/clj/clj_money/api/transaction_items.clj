(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require
            [environ.core :refer [env]]
            [clj-time.format :as f]
            [clj-time.coerce :refer [to-sql-date]]
            [clj-money.api :refer [->response]]
            [clj-money.coercion :as coercion]
            [clj-money.models.transactions :as transactions]
            [clj-money.permissions.transactions]))

(def ^:private criteria-coercion-rules
  [(coercion/rule :integer [:account-id])])

(defn- parse-date
  [string-date]
  (to-sql-date (f/parse-local (:date f/formatters) string-date)))

(defn- coerce-transaction-date
  [criteria]
  (if (:transaction-date criteria)
    (update-in criteria
               [:transaction-date]
               (fn [[operator start end]]
                 [(keyword operator)
                  (parse-date start)
                  (parse-date end)]))
    criteria))

(defn- prepare-criteria
  [criteria]
  (-> criteria
      (coercion/coerce criteria-coercion-rules)
      coerce-transaction-date))

(coercion/register-coerce-fn :sort (fn [s]
                                     (when (and (s (seq s)))
                                       (map keyword s))))

(def ^:private options-coercion-rules
  [(coercion/rule :keyword [:sort 0])
   (coercion/rule :keyword [:sort 1])])

(defn- prepare-options
  [options]
  (coercion/coerce options options-coercion-rules))

(defn index
  [{{:keys [criteria options]} :params}]
  (->response (transactions/search-items (env :db)
                                         (prepare-criteria criteria)
                                         (prepare-options options))))
