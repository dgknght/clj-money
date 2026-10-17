(ns clj-money.api.reconciliations
  (:refer-clojure :exclude [update])
  (:require [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if
                                          parse-bool]]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [dgknght.app-lib.api :as api]
            [clj-money.dates :as dates]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.entities.transaction-items :as itms]
            [clj-money.entities.reconciliations :as recs]
            [clj-money.authorization.reconciliations]))

(defn- symbolic-comparatives
  [m]
  (let [lower (get-in m [:reconciliation/end-of-period-on-or-after])
        upper (get-in m [:reconciliation/end-of-period-on-or-before])]
    (cond-> (dissoc m
                    :reconciliation/end-of-period-on-or-before
                    :reconciliation/end-of-period-on-or-after)
      (and upper lower) (assoc :reconciliation/end-of-period [:between lower upper])
      upper (assoc :reconciliation/end-of-period [:<= upper])
      lower (assoc :reconciliation/end-of-period [:>= lower]))))

(defn- extract-criteria
  [{:keys [params authenticated]}]
  (-> params
      (update-keys util/<-url-safe-keyword)
      (update-in-if [:reconciliation/end-of-period-on-or-before] dates/unserialize-local-date)
      (update-in-if [:reconciliation/end-of-period-on-or-after] dates/unserialize-local-date)
      (symbolic-comparatives)
      (rename-keys {:account-id :reconciliation/account})
      (select-keys [:reconciliation/account
                    :reconciliation/status
                    :reconciliation/end-of-period])
      (update-in-if [:reconciliation/status] util/ensure-keyword)
      (+scope :reconciliation authenticated)))

(defn- translate-sort
  [{:keys [asc desc] :as opts}]
  (cond-> opts
    asc  (assoc :sort [[(keyword "reconciliation" asc) :asc]])
    desc (assoc :sort [[(keyword "reconciliation" desc) :desc]])))

(defn- extract-options
  [{:keys [params]}]
  (-> params
      translate-sort
      (select-keys [:limit :sort])
      (update-in-if [:limit] parse-long)))

(defn- index
  [req]
  (-> req
      extract-criteria
      (entities/select (extract-options req))
      api/response))

(defn- refine-item
  [item]
  (update-in-if item [:transaction/transaction-date] dates/ensure-local-date))

(defn- refine-items
  [items]
  (map refine-item items))

(defn- extract-recon
  [{:keys [params]}]
  (-> params
      (update-in-if [:reconciliation/end-of-period] dates/ensure-local-date)
      (update-in-if [:reconciliation/status] util/ensure-keyword)
      (update-in-if [:reconciliation/balance] bigdec)
      (update-in-if [:reconciliation/items] refine-items)
      (update-in-if [:reconciliation/include-children?] parse-bool)
      (update-in-if [:reconciliation/statement] #(some-> % :id util/->entity-ref))
      (select-keys [:reconciliation/end-of-period
                    :reconciliation/balance
                    :reconciliation/status
                    :reconciliation/items
                    :reconciliation/include-children?
                    :reconciliation/statement])))

(defn- authorize-statement
  "Ensures the user may see the image given as the statement, as anyone
  who can see the reconciliation can see its statement"
  [{:reconciliation/keys [statement] :as recon} user]
  (when statement
    (when-not (some-> (entities/find statement)
                      (authorize ::auth/show user))
      (throw (ex-info "not found" {:type ::auth/not-found
                                   :entity :image
                                   ::auth/opaque? true}))))
  recon)

(defn- accept-reviewed-transactions
  "Completing a reconciliation is the user's review of any transactions in it
  that were read from a statement, so they are marked as accepted"
  [{:reconciliation/keys [status] :as recon}]
  (when (= :completed status)
    (some->> (entities/select
               (util/entity-type
                 {:transaction-item/reconciliation (util/->entity-ref recon)}
                 :transaction))
             (filter #(= :pending (:transaction/review-status %)))
             (map #(assoc % :transaction/review-status :accepted))
             seq
             entities/put-many))
  recon)

(defn- create
  [{:keys [authenticated params] :as req}]
  (-> req
      extract-recon
      (update-in-if [:reconciliation/items] itms/resolve-refs)
      (assoc :reconciliation/account {:id (:account-id params)})
      (authorize ::auth/create authenticated)
      (authorize-statement authenticated)
      entities/put
      accept-reviewed-transactions
      api/creation-response))

(defn- previous-balance
  [{:keys [params authenticated]}]
  (let [account {:id (:account-id params)}
        include-children? (when-let [i (:include-children params)]
                            (parse-bool i))]
    (authorize {:reconciliation/account account} ::auth/show authenticated)
    (api/response {:reconciliation/balance (or (recs/previous-balance account :include-children? include-children?) 0M)})))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (some-> params
          (select-keys [:id])
          (+scope :reconciliation authenticated)
          entities/find-by
          (authorize action authenticated)))

(defn- update
  [{:keys [authenticated] :as req}]
  (or (some-> req
              (find-and-auth ::auth/update)
              (merge (extract-recon req))
              (authorize-statement authenticated)
              (update-in-if [:reconciliation/items] itms/resolve-refs)
              entities/put
              accept-reviewed-transactions
              api/update-response)
      api/not-found))

(def routes
  [["accounts/:account-id/reconciliations" {:get {:handler index}
                                            :post {:handler create}}]
   ["accounts/:account-id/reconciliations/previous-balance" {:get {:handler previous-balance}}]
   ["reconciliations/:id" {:patch {:handler update}}]])
