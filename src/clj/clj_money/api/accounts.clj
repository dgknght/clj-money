(ns clj-money.api.accounts
  (:refer-clojure :exclude [update])
  (:require [environ.core :refer [env]]
            [clj-money.api :refer [->response
                                   index-resource
                                   create-resource
                                   update-resource
                                   delete-resource]]
            [clj-money.authorization :refer [authorize]]
            [clj-money.models.accounts :as accounts]
            [clj-money.permissions.accounts]))

(defn index
  [{params :params}]
  (index-resource accounts/search
                  (select-keys params [:entity-id])
                  :account))

(defn get-one
  [{{id :id} :params}]
  (let [account (authorize (accounts/find-by-id (env :db) id) :show)]
    (->response account)))

(def ^:private attribute-keys
  [:id
   :name
   :entity-id
   :type
   :commodity-id
   :tags
   :parent-id])

(defn- before-save
  [account]
  (-> account
      (update-in [:tags] #(if (:trading account)
                            (conj (or % #{}) :trading)
                            %))
      (select-keys attribute-keys)))

(defn create
  [{params :params}]
  (create-resource :account
                   (before-save params)
                   accounts/create))

(defn update
  [{params :params}]
  (update-resource (before-save params)
                   #(accounts/find-by-id %1 (:id %2))
                   accounts/update))

(defn delete
  [{{id :id} :params}]
  (delete-resource id accounts/find-by-id accounts/delete))
