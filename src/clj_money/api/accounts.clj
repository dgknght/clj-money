(ns clj-money.api.accounts
  (:refer-clojure :exclude [update])
  (:require [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if
                                          parse-bool]]
            [dgknght.app-lib.api :as api]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.authorization :as auth :refer [authorize
                                                      +scope]]
            [clj-money.authorization.accounts]))

(defn- ->coll
  [value]
  (if (coll? value)
    value
    [value]))

(defn- tag-criteria
  [tags]
  [:&&
   (->> (->coll tags)
        (map keyword)
        set)
   :text])

(defn- extract-criteria
  [{:keys [params authenticated]}]
  (-> params
      (util/qualify-keys :account) 
      (rename-keys {:account/entity-id :account/entity})
      (select-keys [:account/entity
                    :account/system-tags
                    :account/user-tags
                    :account/name])
      (update-in-if [:account/entity] #(hash-map :id %))
      (update-in-if [:account/system-tags] tag-criteria)
      (update-in-if [:account/user-tags] tag-criteria)
      (+scope authenticated)))

(defn- extract-options
  [{:keys [params]}]
  (-> params
      (select-keys [:include-children?])
      (update-in-if [:include-children?] parse-bool)))

(defn- index
  [req]
  (api/response
    (entities/select (extract-criteria req)
                   (extract-options req))))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (some-> params
          (select-keys [:id])
          (+scope :account authenticated)
          entities/find-by
          (authorize action authenticated)))

(defn- show
  [req]
  (if-let [account (find-and-auth req ::auth/show)]
    (api/response account)
    api/not-found))

; TODO: Use schema instead of refining these here
(def ^:private attribute-keys
  [:account/name
   :account/entity
   :account/type
   :account/commodity
   :account/system-tags
   :account/user-tags
   :account/parent
   :account/allocations])

(defn- create
  [{:keys [params authenticated]}]
  (-> params
      (select-keys attribute-keys)
      (assoc :account/entity {:id (:entity-id params)})
      (authorize ::auth/create authenticated)
      entities/put
      api/creation-response))

(defn- update
  [{:keys [params] :as req}]
  (if-let [account (find-and-auth req ::auth/update)]
    (-> account
        (merge (select-keys params attribute-keys))
        entities/put
        api/update-response)
    api/not-found))

(defn- delete
  [req]
  (if-let [account (find-and-auth req ::auth/destroy)]
    (do
      (entities/delete account)
      (api/response))
    api/not-found))

(def routes
  [["entities/:entity-id/accounts" {:get {:handler index}
                                    :post {:handler create}}]
   ["accounts/:id" {:get {:handler show}
                    :patch {:handler update}
                    :delete {:handler delete}}]])
