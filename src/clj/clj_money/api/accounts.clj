(ns clj-money.api.accounts
  (:refer-clojure :exclude [update])
  (:require [clojure.set :refer [rename-keys]]
            [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [stowaway.core :as storage]
            [clj-money.api :refer [->response
                                   creation-response
                                   not-found]]
            [clj-money.util :refer [update-in-if
                                    parse-bool
                                    ->coll]]
            [clj-money.models :as models]
            [clj-money.authorization :refer [authorize
                                             +scope]
             :as authorization]
            [clj-money.models.accounts :as accounts]
            [clj-money.authorization.accounts]))

(defn- extract-criteria
  [{:keys [params authenticated]}]
  (-> params
      (rename-keys {"tags[]" :tags})
      (select-keys [:entity-id :tags])
      (update-in-if [:tags] (fn [tags]
                              [:& (->> (->coll tags)
                                       (map keyword)
                                       set)]))
      (+scope ::models/account authenticated)))

(defn- extract-options
  [{:keys [params]}]
  (-> params
      (select-keys [:include-children?])
      (update-in-if [:include-children?] parse-bool)))

(defn- index
  [req]
  (->response (accounts/search (extract-criteria req)
                               (extract-options req))))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (authorize (accounts/find (:id params))
             action
             authenticated))

(defn- show
  [req]
  (->response (find-and-auth req ::authorization/show)))

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
      (update-in [:type] keyword)
      (select-keys attribute-keys)
      (storage/tag ::models/account)))

(defn- create
  [{:keys [params body authenticated]}]
  (let [account (-> body
                    (assoc :entity-id (:entity-id params))
                    before-save
                    (authorize ::authorization/create authenticated)
                    accounts/create)]
    (creation-response account)))

(defn- update
  [{:keys [body] :as req}]
  (if-let [account (find-and-auth req ::authorization/update)]
    (->response (accounts/update
                  (merge account (-> body
                                     (select-keys attribute-keys)
                                     before-save))))
    (not-found)))

(defn- delete
  [req]
  (if-let [account (find-and-auth req ::authorization/destroy)]
    (do
      (accounts/delete account)
      (->response))
    (not-found)))

(defroutes routes
  (GET "/api/entities/:entity-id/accounts" req (index req))
  (POST "/api/entities/:entity-id/accounts" req (create req))
  (GET "/api/accounts/:id" req (show req))
  (PATCH "/api/accounts/:id" req (update req))
  (DELETE "/api/accounts/:id" req (delete req)))
