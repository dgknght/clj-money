(ns clj-money.api.accounts
  (:refer-clojure :exclude [update])
  (:require [clojure.set :refer [rename-keys]]
            [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [stowaway.core :as storage]
            [dgknght.app-lib.core :refer [update-in-if
                                          parse-bool]]
            [dgknght.app-lib.api :as api]
            [clj-money.models :as models]
            [dgknght.app-lib.authorization :refer [authorize
                                                   +scope]
             :as authorization]
            [clj-money.models.accounts :as accounts]
            [clj-money.authorization.accounts]))

(defn- ->coll
  [value]
  (if (coll? value)
    value
    [value]))

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
  (api/response
    (accounts/search (extract-criteria req)
                     (extract-options req))))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (some-> params
          (select-keys [:id])
          (+scope ::models/account authenticated)
          accounts/find-by
          (authorize action authenticated)))

(defn- show
  [req]
  (if-let [account (find-and-auth req ::authorization/show)]
    (api/response account)
    api/not-found))

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
  (-> body
      (assoc :entity-id (:entity-id params))
      before-save
      (authorize ::authorization/create authenticated)
      accounts/create
      api/creation-response))

(defn- update
  [{:keys [body] :as req}]
  (if-let [account (find-and-auth req ::authorization/update)]
    (-> account
        (merge (-> body
                   (select-keys attribute-keys)
                   before-save))
        accounts/update
        api/update-response)
    api/not-found))

(defn- delete
  [req]
  (if-let [account (find-and-auth req ::authorization/destroy)]
    (do
      (accounts/delete account)
      (api/response))
    api/not-found))

(defroutes routes
  (GET "/api/entities/:entity-id/accounts" req (index req))
  (POST "/api/entities/:entity-id/accounts" req (create req))
  (GET "/api/accounts/:id" req (show req))
  (PATCH "/api/accounts/:id" req (update req))
  (DELETE "/api/accounts/:id" req (delete req)))
