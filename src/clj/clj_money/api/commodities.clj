(ns clj-money.api.commodities
  (:refer-clojure :exclude [update count])
  (:require [environ.core :refer [env]]
            [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [clj-money.api :refer [->response]]
            [clj-money.models :as models]
            [clj-money.authorization :refer [authorize
                                             +scope]
             :as authorization]
            [clj-money.validation :as v]
            [clj-money.models.commodities :as coms]
            [clj-money.authorization.commodities]))

(defn- scoped-params
  [{:keys [params authenticated]}]
  (-> params
      (select-keys [:entity-id])
      (+scope ::models/commodity authenticated)))

(defn- count
  [req]
  (->response
    {:count (coms/count (env :db)
                        (scoped-params req))}))

(defn- index
  [req]
  (->response
    (coms/search (env :db)
                 (scoped-params req))))

(defn- find-and-authorize
  [{:keys [params authenticated]} action]
  (authorize (coms/find-by-id (env :db) (:id params))
             action
             authenticated))

(defn- show
  [req]
  (->response (find-and-authorize req ::authorization/show)))

(def ^:private attribute-keys
  [:id
   :entity-id
   :name
   :symbol
   :exchange
   :type])

(defn- ensure-keyword
  [m & ks]
  (reduce #(if (contains? %1 %2)
             (update-in %1 [%2] keyword)
             m)
          m
          ks))

(defn- create
  [{:keys [body authenticated params]}]
  (let [commodity (-> body
                      (assoc :entity-id (:entity-id params))
                      (select-keys attribute-keys)
                      (ensure-keyword :exchange :type)
                      (models/tag ::models/commodity)
                      (authorize ::authorization/create authenticated))
        result (coms/create (env :db) commodity)]
    (->response result
                (if (v/has-error? result)
                  400
                  201))))

(defn- update
  [{:keys [body] :as req}]
  (let [commodity (find-and-authorize req ::authorization/update)]
    (->response (coms/update (env :db) (merge commodity (-> body
                                                            (select-keys attribute-keys)
                                                            (ensure-keyword :exchange :type)))))))

(defn- delete
  [req]
  (let [commodity (find-and-authorize req ::authorization/destroy)]
    (coms/delete (env :db) (:id commodity))
    (->response)))

(defroutes routes
  (GET "/api/entities/:entity-id/commodities/count" req (count req))
  (GET "/api/entities/:entity-id/commodities" req (index req))
  (POST "/api/entities/:entity-id/commodities" req (create req))
  (GET "/api/commodities/:id" req (show req))
  (PATCH "/api/commodities/:id" req (update req))
  (DELETE "/api/commodities/:id" req (delete req)))
