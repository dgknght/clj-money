(ns clj-money.api.transactions
  (:refer-clojure :exclude [update])
  (:require [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [stowaway.core :as storage]
            [clj-money.util :refer [uuid
                                    update-in-if
                                    unserialize-date]]
            [clj-money.api :refer [->response
                                   creation-response]]
            [clj-money.models :as models]
            [clj-money.authorization :refer [authorize
                                             +scope]
             :as authorization]
            [clj-money.models.transactions :as trans]
            [clj-money.authorization.transactions]))

(defn- index
  [{:keys [params authenticated]}]
  (->response
    (trans/search (-> params
                      (assoc :transaction-date [:between
                                                (unserialize-date (:start params))
                                                (unserialize-date (:end params))])
                      (select-keys [:entity-id :transaction-date])
                      (+scope ::models/transaction authenticated)))))

(defn- show
  [{{:keys [id transaction-date]} :params authenticated :authenticated}]
  (->response (authorize (trans/find id
                                     (unserialize-date transaction-date))
                         ::authorization/show
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

(defn- parse-item
  [item]
  (update-in-if item [:action] keyword))

(defn- create
  [{:keys [params body authenticated]}]
  (creation-response (-> body
                         (update-in-if [:transaction-date] unserialize-date)
                         (update-in-if [:items] #(map parse-item %))
                         (select-keys attribute-keys)
                         (assoc :entity-id (:entity-id params))
                         (storage/tag ::models/transaction)
                         (authorize ::authorization/create authenticated)
                         trans/create)))

(defn- apply-to-existing
  [updated-item items]
  (let [parsed (parse-item updated-item)]
    (if-let [existing (->> items
                           (filter #(= (:id %) (:id updated-item)))
                           first)]
      (merge existing parsed)
      parsed)))

(defn- apply-item-updates
  [items updates]
  (map #(apply-to-existing % items) updates))

(defn- apply-update
  [transaction body]
  (-> transaction
      (merge (-> body
                 (update-in-if [:transaction-date] unserialize-date)
                 (update-in-if [:id] uuid)))
      (select-keys attribute-keys)
      (update-in [:items] apply-item-updates (:items body))))

(defn- update
  [{:keys [params body authenticated]}]
  (let [trans-date (some #(params %) [:original-transaction-date :transaction-date])
        transaction (authorize (trans/find (:id params) trans-date)
                               ::authorization/update
                               authenticated)]
    (->response (trans/update (apply-update transaction body)))))

(defn- delete
  [{:keys [params authenticated]}]
  (-> (trans/find (:id params)
                  (:transaction-date params))
      (authorize ::authorization/destroy authenticated)
      trans/delete)
  (->response))

(defroutes routes
  (GET "/api/entities/:entity-id/:start/:end/transactions" req (index req))
  (POST "/api/entities/:entity-id/transactions" req (create req))
  (GET "/api/transactions/:transaction-date/:id" req (show req))
  (PATCH "/api/transactions/:transaction-date/:id" req (update req))
  (DELETE "/api/transactions/:transaction-date/:id" req (delete req)))
