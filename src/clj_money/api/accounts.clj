(ns clj-money.api.accounts
  (:refer-clojure :exclude [update])
  (:require [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if
                                          parse-int
                                          parse-bool]]
            [dgknght.app-lib.api :as api]
            [clj-money.util :as util]
            [clj-money.models :as models]
            [clj-money.db :as db]
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
  [:& (->> (->coll tags)
           (map keyword)
           set)])

(defn- extract-criteria
  [{:keys [params authenticated]}]
  (-> params
      (util/qualify-keys :account) 
      (rename-keys {"system-tags[]" :account/system-tags
                    "user-tags[]" :account/user-tags
                    :account/entity-id :account/entity})
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
    (models/select (extract-criteria req)
                   (extract-options req))))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (some-> params
          (select-keys [:id])
          (db/model-type :account)
          (+scope ::models/account authenticated)
          models/find-by
          (authorize action authenticated)))

(defn- show
  [req]
  (if-let [account (find-and-auth req ::auth/show)]
    (api/response account)
    api/not-found))

(def ^:private attribute-keys
  [:id
   :account/name
   :account/entity
   :account/type
   :account/commodity
   :account/system-tags
   :account/user-tags
   :account/parent
   :account/allocations])

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
    account))

; JSON serialization/deserialization wants the keys to be keywords
(defn- correct-allocations
  [allocations]
  (reduce (fn [r [k v]]
            (assoc r (-> k name parse-int) (bigdec v)))
          {}
          allocations))

(defn- before-save
  [account]
  (-> account
      (update-in-if [:account/user-tags] prepare-tags)
      (update-in-if [:account/system-tags] prepare-tags)
      (update-in-if [:account/type] keyword)
      (update-in-if [:account/allocations] correct-allocations)
      handle-trading-tag
      (select-keys attribute-keys)))

(defn- create
  [{:keys [params body authenticated]}]
  (-> body
      (assoc :account/entity {:id (:entity-id params)})
      before-save
      (authorize ::auth/create authenticated)
      models/put
      api/creation-response))

(defn- update
  [{:keys [body] :as req}]
  (if-let [account (find-and-auth req ::auth/update)]
    (-> account
        (merge (before-save body))
        models/put
        api/update-response)
    api/not-found))

(defn- delete
  [req]
  (if-let [account (find-and-auth req ::auth/destroy)]
    (do
      (models/delete account)
      (api/response))
    api/not-found))

(def routes
  [["entities/:entity-id/accounts" {:get {:handler index}
                                    :post {:handler create}}]
   ["accounts/:id" {:get {:handler show}
                    :patch {:handler update}
                    :delete {:handler delete}}]])
