(ns clj-money.api.statement-ingestions
  (:refer-clojure :exclude [update])
  (:require [dgknght.app-lib.api :as api]
            [dgknght.app-lib.core :refer [update-in-if
                                          parse-bool]]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [clj-money.util :as util :refer [id=]]
            [clj-money.dates :as dates]
            [clj-money.entities :as entities]
            [clj-money.entities.images :as img]
            [clj-money.ingestion.statement-reconciliation :as reconciliation]
            [clj-money.web.system :as system]
            [clj-money.authorization.statement-ingestions]))

(defn- find-account
  "Returns the account, if it belongs to the entity and is visible to the
  user"
  [account-id entity user]
  (some-> account-id
          (#(util/entity-type {:id %} :account))
          (+scope :account user)
          entities/find-by
          (#(when (id= entity (:account/entity %)) %))))

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
  [{{:keys [account-id entity-id]} :params :keys [authenticated] :as req}]
  (let [account (find-account account-id {:id entity-id} authenticated)]
    (if (and account-id (not account))
      api/not-found
      (create* req account))))

(defn- show
  [{:keys [params authenticated]}]
  (or (some-> params
              (select-keys [:id])
              (+scope :statement-ingestion authenticated ::auth/manage)
              entities/find-by
              (authorize ::auth/show authenticated)
              api/response)
      api/not-found))

(defn- ->date-str
  [d]
  (dates/serialize-local-date (dates/ensure-local-date d)))

(defn- extract-corrections
  "Returns the parts of the statement the user corrected, in the form in
  which the model returned them. A part left blank keeps what the model
  read."
  [{{:statement-ingestion/keys [statement]} :params}]
  (-> (into {}
            (filter val)
            (select-keys statement [:period-start
                                    :period-end
                                    :opening-balance
                                    :closing-balance]))
      (update-in-if [:period-start] ->date-str)
      (update-in-if [:period-end] ->date-str)
      (update-in-if [:opening-balance] bigdec)
      (update-in-if [:closing-balance] bigdec)))

(defn- correct
  "Applies the corrections to a statement that couldn't be reconciled, and
  reconciles it again, replacing the account's reconciliation in progress
  if :statement-ingestion/replace? is true"
  [{:statement-ingestion/keys [entity] :as ingestion}
   {:keys [params authenticated] :as req}]
  (let [account-id (get-in params [:statement-ingestion/account :id])
        account (some-> account-id (find-account entity authenticated))]
    (if (and account-id (not account))
      api/not-found
      (let [updated (entities/put
                      (cond-> (-> ingestion
                                  (update-in [:statement-ingestion/statement]
                                             merge
                                             (extract-corrections req))
                                  (assoc :statement-ingestion/status :pending))
                        account (assoc :statement-ingestion/account
                                       (util/->entity-ref account))))]
        ; reconciling the statement takes a while, so the client polls for
        ; the result
        (future (reconciliation/reconcile-statement
                  updated
                  :replace? (parse-bool (:statement-ingestion/replace? params))))
        (api/update-response updated)))))

(defn- update
  "Corrects the statement read for an ingestion that failed, or that awaits
  confirmation that the reconciliation in progress should be replaced, and
  reconciles it again. Only an ingestion with a statement that was read can
  be corrected."
  [{:keys [params authenticated] :as req}]
  (if-let [ingestion (some-> params
                             (select-keys [:id])
                             (+scope :statement-ingestion authenticated ::auth/manage)
                             entities/find-by
                             (authorize ::auth/update authenticated))]
    (if (and (#{:failed :conflict} (:statement-ingestion/status ingestion))
             (:statement-ingestion/statement ingestion))
      (correct ingestion req)
      api/unprocessable)
    api/not-found))

(def routes
  [["entities/:entity-id/statement-ingestions" {:post {:handler create}}]
   ["statement-ingestions/:id" {:get {:handler show}
                                :patch {:handler update}}]])
