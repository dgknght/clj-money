(ns clj-money.api.commodities
  (:refer-clojure :exclude [update])
  (:require [environ.core :refer [env]]
            [clj-money.authorization :refer [authorize]]
            [clj-money.api :refer [->response
                                   index-resource
                                   create-resource
                                   update-resource
                                   delete-resource]]
            [clj-money.models.commodities :as commodities]
            [clj-money.permissions.commodities]))

(defn index
  [{params :params}]
  (index-resource commodities/search
                  (select-keys params [:entity-id])
                  :commodity))

(defn get-one
  [{{id :id} :params}]
  (->response (authorize (commodities/find-by-id (env :db) id) :show)))

(def ^:private attribute-keys
  [:id
   :entity-id
   :name
   :symbol
   :exchange
   :type])

(defn create
  [{params :params}]
  (create-resource :commodity
                   (select-keys params attribute-keys)
                   commodities/create))

(defn update
  [{params :params}]
  (update-resource (select-keys params attribute-keys)
                   #(commodities/find-by-id %1 (:id %2))
                   commodities/update))

(defn delete
  [{{id :id} :params}]
  (delete-resource id commodities/find-by-id commodities/delete))
