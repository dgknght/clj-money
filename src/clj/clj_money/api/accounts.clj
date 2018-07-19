(ns clj-money.api.accounts
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
            [clj-money.models.accounts :as accounts]
            [clj-money.permissions.accounts]))

(defn index
  [{params :params}]
  (index-resource accounts/search
                  (select-keys params [:entity-id])
                  :account))

(defn get-account
  [{{id :id} :params}]
  ; TODO add authorization here
  (->response (accounts/find-by-id (env :db) id)))

(def ^:private attribute-keys
  [:id
   :name
   :entity-id
   :type
   :commodity-id
   :parent-id])

(defn create
  [{params :params}]
  (create-resource :account
                   (select-keys params attribute-keys)
                   accounts/create))

(defn update
  [{params :params}]
  (update-resource (select-keys params attribute-keys)
                   accounts/find-by-id
                   accounts/update))

(defn delete
  [{{id :id} :params}]
  (delete-resource id accounts/find-by-id accounts/delete))
