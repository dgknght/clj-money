(ns clj-money.api.transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [stowaway.core :as storage]
            [dgknght.app-lib.core :refer [uuid
                                     update-in-if]]
            [dgknght.app-lib.api :as api]
            [dgknght.app-lib.authorization :refer [authorize
                                             +scope]
             :as authorization]
            [clj-money.dates :refer [unserialize-local-date]]
            [clj-money.models :as models]
            [clj-money.models.transactions :as trans]
            [clj-money.authorization.transactions]
            [clj-money.transactions :refer [expand]]))

(defn- ->criteria
  [{:keys [params authenticated]}]
  (-> params
      (assoc :transaction-date [:between
                                (unserialize-local-date (:start params))
                                (unserialize-local-date (:end params))])
      (select-keys [:entity-id :transaction-date])
      (+scope ::models/transaction authenticated)))

(defn- ->options
  [{:keys [params]}]
  (-> params
      (select-keys [:include-items])
      (rename-keys {:include-items :include-items?})))

(defn- index
  [req]
  (api/response
   (trans/search (->criteria req) (->options req))))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (let [trans-date (unserialize-local-date
                     (some #(params %)
                           [:original-transaction-date
                            :transaction-date]))]
    (some-> params
            (select-keys [:id])
            (update-in [:id] uuid)
            (assoc :transaction-date trans-date)
            (storage/tag ::models/transaction)
            (+scope authenticated)
            trans/find-by
            (authorize action authenticated))))

(defn- show
  [req]
  (if-let [trx (find-and-auth req ::authorization/show)]
    (api/response trx)
    api/not-found))

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
  (-> item
      (update-in-if [:quantity] bigdec)
      (update-in-if [:value] bigdec)
      (update-in-if [:action] keyword)))

(defn- create
  [{:keys [params body authenticated]}]
  (api/creation-response
    (-> body
        expand
        (update-in-if [:transaction-date] unserialize-local-date)
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
                 (update-in-if [:transaction-date] unserialize-local-date)
                 (update-in-if [:original-transaction-date] unserialize-local-date)
                 (update-in-if [:id] uuid)))
      (select-keys attribute-keys)
      (update-in [:items] apply-item-updates (:items body))))

(defn- update
  [{:keys [body] :as req}]
  (if-let [transaction (find-and-auth req ::authorization/update)]
    (-> transaction
        (apply-update body)
        trans/update
        api/update-response)
    api/not-found))

(defn- delete
  [req]
  (if-let [transaction (find-and-auth req ::authorization/destroy)]
    (do
      (trans/delete transaction)
      (api/response))
    api/not-found))

(def routes
  [["entities/:entity-id"
    ["/transactions" {:post {:handler create}}]
    ["/:start/:end/transactions" {:get {:handler index}}]]
   ["transactions/:transaction-date/:id" {:get {:handler show}
                                          :patch {:handler update}
                                          :delete {:handler delete}}]])
