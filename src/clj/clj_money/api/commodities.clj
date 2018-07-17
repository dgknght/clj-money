(ns clj-money.api.commodities
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [ring.util.response :refer [status response header]]
            [slingshot.slingshot :refer [try+]]
            [clj-money.api :refer [->response
                                   error->response
                                   update-resource
                                   delete-resource]]
            [clj-money.validation :as validation]
            [clj-money.authorization :refer [authorize
                                             apply-scope
                                             tag-resource]]
            [clj-money.models.commodities :as commodities]
            [clj-money.permissions.commodities]))

(defn index
  [{{entity-id :entity-id} :params}]
  (->response (commodities/search
                (env :db)
                (apply-scope {:entity-id entity-id} :commodity))))

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
  (update-resource (select-keys params [:id
                                        :entity-id
                                        :symbol
                                        :name
                                        :exchange
                                        :type])
                   commodities/find-by-id
                   commodities/update))

(defn delete
  [{{id :id} :params}]
  (delete-resource id commodities/find-by-id commodities/delete))
