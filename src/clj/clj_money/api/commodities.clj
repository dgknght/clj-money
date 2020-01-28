(ns clj-money.api.commodities
  (:refer-clojure :exclude [update])
  (:require [environ.core :refer [env]]
            [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [clj-money.api :refer [->response]]
            [clj-money.authorization :refer [authorize
                                             tag-resource
                                             apply-scope]]
            [clj-money.models.commodities :as coms]
            [clj-money.permissions.commodities]))

(defn- index
  [{:keys [params authenticated]}]
  (->response (coms/search (env :db) (-> params
                                         (select-keys [:entity-id])
                                         (apply-scope :commodity authenticated)))))

(defn- find-and-authorize
  [{:keys [params authenticated]} action]
  (authorize (coms/find-by-id (env :db) (:id params))
             action
             authenticated))

(defn- show
  [req]
  (->response (find-and-authorize req :show)))

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
                      (tag-resource :commodity)
                      (authorize :create authenticated))]
    (->response (coms/create (env :db) commodity))))

(defn- update
  [{:keys [body] :as req}]
  (let [commodity (find-and-authorize req :update)]
    (->response (coms/update (env :db) (merge commodity (-> body
                                                            (select-keys attribute-keys)
                                                            (ensure-keyword :exchange :type)))))))

(defn- delete
  [req]
  (let [commodity (find-and-authorize req :delete)]
    (coms/delete (env :db) (:id commodity))
    (->response)))

(defroutes routes
  (GET "/api/entities/:entity-id/commodities" req (index req))
  (POST "/api/entities/:entity-id/commodities" req (create req))
  (GET "/api/commodities/:id" req (show req))
  (PATCH "/api/commodities/:id" req (update req))
  (DELETE "/api/commodities/:id" req (delete req)))
