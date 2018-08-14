(ns clj-money.api.transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.spec.alpha :as s]
            [environ.core :refer [env]]
            [ring.util.response :refer [status response header]]
            [clj-money.api :refer [->response
                                   invalid->response
                                   error->response
                                   index-resource
                                   create-resource
                                   update-resource
                                   delete-resource]]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.authorization :refer [authorize
                                             tag-resource]]
            [clj-money.models.transactions :as transactions]
            [clj-money.permissions.transactions]))

(defn index
  [{params :params}]
  (index-resource transactions/search
                  (select-keys params [:entity-id])
                  :transaction))

(defn get-transaction
  [{{id :id} :params}]
  ; TODO add authorization here
  (->response (transactions/find-by-id (env :db) id)))

(def ^:private attribute-keys
  [:id
   :description
   :entity-id
   :transaction-date
   :memo
   :items])

(defn create
  [{params :params}]
  (create-resource :transaction
                   (select-keys params attribute-keys)
                   transactions/create))

(defn update
  [{params :params}]
  (update-resource (select-keys params attribute-keys)
                   transactions/find-by-id
                   transactions/update))

(s/def ::id uuid?)
(s/def ::transaction-date validation/local-date?)
(s/def ::delete-params (s/keys :req-un [::id ::transaction-date]))

(def ^:prtivate delete-coercion-rules
  [(coercion/rule :uuid [:id])
   (coercion/rule :local-date [:transaction-date])])

(defn- before-validation
  [params]
  (coercion/coerce delete-coercion-rules params))

(defn- validate-delete-params
  [params]
  (->> params
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
