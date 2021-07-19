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

(defn- tag-criteria
  [tags]
  [:& (->> (->coll tags)
           (map keyword)
           set)])

(defn- extract-criteria
  [{:keys [params authenticated]}]
  (-> params
      (rename-keys {"system-tags[]" :system-tags
                    "user-tags[]" :user-tags})
      (select-keys [:entity-id :system-tags :user-tags])
      (update-in-if [:system-tags] tag-criteria)
      (update-in-if [:user-tags] tag-criteria)
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
   :system-tags
   :user-tags
   :parent-id])

(defn- prepare-tags
  [tags]
  (if tags
    (->> tags
         (map keyword)
         set)
    #{}))

(defn- handle-trading-tag
  [{:keys [trading] :as account}]
  (if trading
    (update-in account [:system-tags] (fnil conj #{}) :trading)
    (if (seq (:system-tags account))
      (update-in account [:system-tags] disj :trading)
      account)))

(defn- before-save
  [account]
  (-> account
      (update-in-if [:user-tags] prepare-tags)
      (update-in-if [:system-tags] prepare-tags)
      (update-in-if [:type] keyword)
      handle-trading-tag
      (select-keys attribute-keys)))

(defn- create
  [{:keys [params body authenticated]}]
  (-> body
      (assoc :entity-id (:entity-id params))
      before-save
      (storage/tag ::models/account)
      (authorize ::authorization/create authenticated)
      accounts/create
      api/creation-response))

(defn- update
  [{:keys [body] :as req}]
  (if-let [account (find-and-auth req ::authorization/update)]
    (-> account
        (merge (before-save body))
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
