(ns clj-money.api.transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.api :as api]
            [clj-money.comparatives :as comparatives]
            [clj-money.authorization :refer [authorize
                                             +scope]
             :as authorization]
            [clj-money.util :as util :refer [id=]]
            [clj-money.dates :refer [unserialize-local-date]]
            [clj-money.entities :as entities]
            [clj-money.entities.propagation :as prop]
            [clj-money.authorization.transactions]
            [clj-money.transactions :refer [expand]]))

(defn- unserialize-date
  [x]
  (cond
    (vector? x) (mapv unserialize-date x)
    (and (string? x)
         (re-find #"^\d{4}-\d{2}-\d{2}$" x)) (unserialize-local-date x)
    :else x))

(defn- ->criteria
  [{:keys [params authenticated]}]
  (-> params
      comparatives/symbolize
      (update-in-if [:transaction-date] unserialize-date)
      (rename-keys {:transaction-date :transaction/transaction-date})
      (select-keys [:transaction/entity
                    :transaction/transaction-date])
      (+scope :transaction authenticated)))

(defn- ->options
  [{:keys [params]}]
  (-> params
      (select-keys [:include-items])
      (rename-keys {:include-items :include-items?})))

(defn- index
  [req]
  (api/response
   (entities/select (->criteria req) (->options req))))

(defn- find-and-auth
  [{:keys [path-params authenticated]} action]
  (let [trans-date (unserialize-local-date
                     (some #(path-params %)
                           [:original-transaction-date
                            :transaction-date]))]
    (some-> path-params
            (select-keys [:id])
            (assoc :transaction/transaction-date trans-date)
            (+scope authenticated)
            entities/find-by
            (authorize action authenticated))))

(defn- show
  [req]
  (if-let [trx (find-and-auth req ::authorization/show)]
    (api/response trx)
    api/not-found))

(def ^:private attribute-keys
  [:id
   :transaction/description
   :transaction/entity
   :transaction/transaction-date
   :transaction/original-transaction-date
   :transaction/memo
   :transaction/items
   :transaction/debit-account
   :transaction/credit-account
   :transaction/quantity])

(defn- extract-transaction
  [{:keys [params]}]
  (-> params
      (dissoc :id)
      expand
      (select-keys attribute-keys)))

(defn- create
  [{:keys [authenticated params] :as req}]
  (-> req
      extract-transaction
      (assoc :transaction/entity {:id (:entity-id params)})
      (authorize ::authorization/create authenticated)
      prop/put-and-propagate
      api/creation-response))

(defn- apply-to-existing
  [updated-item items]
  (if-let [existing (->> items
                         (filter #(id= % updated-item))
                         first)]
    (merge existing updated-item)
    updated-item))

(defn- apply-item-updates
  [items updates]
  (mapv #(apply-to-existing % items) updates))

(defn- apply-update
  [transaction req]
  (let [updated (extract-transaction req)]
    (-> transaction
        (merge (dissoc updated :transaction/items))
        (select-keys attribute-keys)
        (update-in [:transaction/items]
                   apply-item-updates
                   (:transaction/items updated)))))

(defn- update
  [req]
  (or (some-> (find-and-auth req ::authorization/update)
              (apply-update req)
              prop/put-and-propagate
              api/update-response)
      api/not-found))

(defn- delete
  [req]
  (or (some-> (find-and-auth req ::authorization/destroy)
              prop/delete-and-propagate
              api/response)
      api/not-found))

(def routes
  [["entities/:entity-id"
    ["/transactions" {:post {:handler create}
                      :get {:handler index}}]]
   ["transactions/:transaction-date/:id" {:get {:handler show}
                                          :patch {:handler update}
                                          :delete {:handler delete}}]])
