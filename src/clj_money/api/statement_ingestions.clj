(ns clj-money.api.statement-ingestions
  (:require [dgknght.app-lib.api :as api]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [clj-money.util :as util :refer [id=]]
            [clj-money.entities :as entities]
            [clj-money.entities.images :as img]
            [clj-money.ingestion.statement-reconciliation :as reconciliation]
            [clj-money.web.system :as system]
            [clj-money.authorization.statement-ingestions]))

(defn- find-account
  "Returns the account the statement was uploaded for, if any. The account
  must belong to the entity, and be visible to the user."
  [{{:keys [account-id entity-id]} :params :keys [authenticated]}]
  (some-> account-id
          (#(util/entity-type {:id %} :account))
          (+scope :account authenticated)
          entities/find-by
          (#(when (id= {:id entity-id} (:account/entity %)) %))))

(defn- create*
  [{:keys [authenticated] {:keys [entity-id statement]} :params :as req}
   account]
  (let [ingestion (-> #:statement-ingestion{:entity {:id entity-id}
                                            :status :pending}
                      (authorize ::auth/create authenticated)
                      (assoc :statement-ingestion/image
                             (util/->entity-ref
                               (img/find-or-create-upload statement authenticated)))
                      (cond-> account
                        (assoc :statement-ingestion/account (util/->entity-ref account)))
                      entities/put)]
    ; reading the statement takes a while, so the client polls for the result
    (future (reconciliation/reconcile ingestion
                                     (system/component req :statement-reader)))
    (api/creation-response ingestion)))

(defn- create
  [{{:keys [account-id]} :params :as req}]
  (let [account (find-account req)]
    (if (and account-id (not account))
      api/not-found
      (create* req account))))

(defn- show
  [{:keys [params authenticated]}]
  (or (some-> params
              (select-keys [:id])
              (+scope :statement-ingestion authenticated)
              entities/find-by
              (authorize ::auth/show authenticated)
              api/response)
      api/not-found))

(def routes
  [["entities/:entity-id/statement-ingestions" {:post {:handler create}}]
   ["statement-ingestions/:id" {:get {:handler show}}]])
