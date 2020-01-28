(ns clj-money.api.transactions
  (:refer-clojure :exclude [update])
  (:require [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [environ.core :refer [env]]
            [clj-money.x-platform.util :refer [unserialize-date]]
            [clj-money.api :refer [->response]]
            [clj-money.authorization :refer [authorize
                                             tag-resource
                                             apply-scope]]
            [clj-money.models.transactions :as trans]
            [clj-money.permissions.transactions]))

(defn- index
  [{:keys [params authenticated]}]
  (->response (trans/search (env :db) (-> params
                                          (select-keys [:entity-id :account-id])                        
                                          (apply-scope :transaction authenticated)))))

(defn- show
  [{{:keys [id transaction-date]} :params authenticated :authenticated}]
  (->response (authorize (trans/find-by-id (env :db)
                                       id
                                       (unserialize-date transaction-date))
                         :show
                         authenticated)))

(def ^:private attribute-keys
  [:id
   :description
   :entity-id
   :transaction-date
   :original-transaction-date
   :memo
   :items
   :debit-account-id
   :credit-account-id
   :quantity])

(defn- create
  [{:keys [params body authenticated]}]
  (->response (trans/create (env :db) (-> body
                                          (select-keys attribute-keys)
                                          (assoc :entity-id (:entity-id params))
                                          (tag-resource :transaction)
                                          (authorize :create authenticated)))))

(defn- apply-to-existing
  [updated-item items]
  (if-let [existing (->> items
                         (filter #(= (:id %) (:id updated-item)))
                         first)]
    (merge existing updated-item)
    updated-item))

(defn- apply-item-updates
  [items updates]
  (map #(apply-to-existing % items) updates))

(defn- apply-update
  [transaction body]
  (-> transaction
      (merge body)
      (select-keys attribute-keys)
      (update-in [:items] apply-item-updates (:items body))))

(defn- update
  [{:keys [params body authenticated]}]
  (let [trans-date (some #(params %) [:original-transaction-date :transaction-date])
        transaction (authorize (trans/find-by-id (env :db) (:id params) trans-date)
                               :update
                               authenticated)]
    (->response (trans/update (env :db) (apply-update transaction body)))))

(defn- delete
  [{:keys [params authenticated]}]
  (let [transaction (authorize (trans/find-by-id (env :db)
                                                 (:id params)
                                                 (:transaction-date params))
                               :delete
                               authenticated)]
    (trans/delete (env :db) (:id transaction)  (:transaction-date transaction))
    (->response)))

(defroutes routes
  (GET "/api/entities/:entity-id/transactions" req (index req))
  (POST "/api/entities/:entity-id/transactions" req (create req))
  (GET "/api/transactions/:transaction-date/:id" req (show req))
  (PATCH "/api/transactions/:transaction-date/:id" req (update req))
  (DELETE "/api/transactions/:transaction-date/:id" req (delete req)))
