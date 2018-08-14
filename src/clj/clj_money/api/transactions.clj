(ns clj-money.api.transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [ring.util.response :refer [status response header]]
            [clj-money.api :refer [->response
                                   error->response
                                   index-resource
                                   create-resource
                                   update-resource
                                   delete-resource]]
            [clj-money.validation :as validation]
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

(defn delete
  [{{:keys [id transaction-date] :as params} :params}]

  (pprint {:delete params})

  (let [transaction (authorize (transactions/find-by-id (env :db) transaction-date id) :delete)]
    (try
      (transactions/delete (env :db) transaction-date (:id transaction))
      (catch Exception e
        (error->response e "Unable to delete the transaction.")))
    {:status 204
     :body []}))
