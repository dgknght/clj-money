(ns clj-money.api.commodities
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [ring.util.response :refer [status response header]]
            [cheshire.core :as json]
            [clj-money.api :refer [->response error->response]]
            [clj-money.validation :as validation]
            [clj-money.authorization :refer [authorize
                                             tag-resource]]
            [clj-money.models.commodities :as commodities]
            [clj-money.permissions.commodities]))

(defn index
  [{{entity-id :entity-id} :params}]
  (->response (commodities/search (env :db) {:entity-id entity-id})))

(defn get-commodity
  [{{id :id} :params}]
  (->response (commodities/find-by-id (env :db) id)))

(defn create
  [{params :params}]
  (let [commodity (-> params
                      (select-keys [:entity-id
                                    :name
                                    :symbol
                                    :exchange
                                    :type])
                      (tag-resource :commodity)
                      (authorize :create))
        result (commodities/create (env :db) commodity)]
    (if (validation/has-error? result)
      (status (->response result) 422)
      (status (->response result) 201))))

(defn update
  [{params :params}]
  (let [commodity (authorize (commodities/find-by-id (env :db) (:id params))
                             :update)
        updated (merge commodity (select-keys params [:id
                                                      :entity-id
                                                      :symbol
                                                      :name
                                                      :exchange
                                                      :type]))
        result (commodities/update (env :db) updated)]
    (if (validation/has-error? result)
      (status (->response result) 422)
      (status (->response result) 200))))

(defn delete
  [req]
  (->response []))
