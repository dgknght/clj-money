(ns clj-money.api.transactions
  (:refer-clojure :exclude [update])
  (:require
            [clojure.spec.alpha :as s]
            [clj-time.format :as f]
            [environ.core :refer [env]]
            [clj-money.api :refer [->response
                                   invalid->response
                                   error->response
                                   index-resource
                                   create-resource
                                   update-resource]]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.authorization :refer [authorize]]
            [clj-money.models.transactions :as transactions]
            [clj-money.permissions.transactions]))

(defn index
  [{params :params}]
  (index-resource transactions/search
                  (select-keys params [:entity-id :account-id])
                  :transaction))

(defn get-one
  [{{:keys [id transaction-date]} :params}]
  (->response (authorize (transactions/find-by-id (env :db)
                                       id
                                       (f/parse-local-date (:date f/formatters)
                                                           transaction-date))
                         :show)))

(def ^:private attribute-keys
  [:id
   :description
   :entity-id
   :transaction-date
   :original-transaction-date
   :memo
   :items
   :debit-account-id
   :credit-account-id
   :quantity])

(defn create
  [{params :params}]
  (create-resource :transaction
                   (select-keys params attribute-keys)
                   transactions/create))

(defn update
  [{params :params}]
  (update-resource (select-keys params attribute-keys)
                   (fn [s {:keys [id transaction-date original-transaction-date]}]
                     (transactions/find-by-id s id (or original-transaction-date
                                                       transaction-date)))
                   transactions/update))

(s/def ::id uuid?)
(s/def ::transaction-date validation/local-date?)
(s/def ::delete-params (s/keys :req-un [::id ::transaction-date]))

(def ^:private delete-coercion-rules
  [(coercion/rule :uuid [:id])
   (coercion/rule :local-date [:transaction-date])])

(defn- before-validation
  [params]
  (coercion/coerce params delete-coercion-rules))

(defn- validate-delete-params
  [params]
  (-> params
      before-validation
      (validation/validate ::delete-params)))

(defn delete
  [{params :params}]
  (let [{:keys [id transaction-date]
         :as validated} (validate-delete-params params)]
    (if (validation/has-error? validated)
      (invalid->response validated)
      (let [transaction (authorize (transactions/find-by-id (env :db)
                                                            id
                                                            transaction-date)
                                   :delete)]
        (try
          (transactions/delete (env :db) (:id transaction) transaction-date)
          (catch Exception e
            (error->response e "Unable to delete the transaction.")))
        {:status 204
         :body []}))))
