(ns clj-money.api.receipt-ingestions
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [dgknght.app-lib.api :as api]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [clj-money.util :as util]
            [clj-money.images :as images]
            [clj-money.entities :as entities]
            [clj-money.entities.images :as img]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.receipts :as rcpts]
            [clj-money.web.system :as system]
            [clj-money.authorization.receipt-ingestions]))

(defn- read-receipt
  [ingestion reader]
  (let [{:receipt-ingestion/keys [entity image] :as ingestion}
        (entities/put (assoc ingestion :receipt-ingestion/status :processing))
        entity (entities/find entity)]
    (try
      (let [account-opts (rcpts/account-options entity)
            receipt (ing/read-receipt reader
                                      (images/get (:image/uuid (entities/find image)))
                                      entity
                                      {:account-options account-opts})
            trx (assoc (rcpts/make-trx receipt
                                       entity
                                       :account-options account-opts)
                       :id (util/temp-id))
            att #:attachment{:transaction trx
                             :image image
                             :caption "Receipt"}
            [_ _ saved] (entities/put-many
                          [trx
                           att
                           (assoc ingestion
                                  :receipt-ingestion/transaction (util/->entity-ref trx)
                                  :receipt-ingestion/receipt receipt)])]
        (-> saved
            (assoc :receipt-ingestion/status :complete)
            entities/put))
      (catch Exception e
        (log/errorf e "[receipt-ingestion] unable to read receipt %s" (:id ingestion))
        (entities/put (assoc ingestion
                             :receipt-ingestion/status :failed
                             :receipt-ingestion/error (ex-message e)))))))

(defn- create-image
  [{{:keys [image]} :params
    :keys [authenticated]}]
  (util/->entity-ref
    (img/find-or-create-upload image authenticated)))

(defn- create
  [{:keys [authenticated] {:keys [entity-id]} :params :as req}]
  (let [ingestion (-> #:receipt-ingestion{:entity {:id entity-id}
                                          :status :pending}
                      (authorize ::auth/create authenticated)
                      (assoc :receipt-ingestion/image (create-image req))
                      entities/put)]
    ; reading the receipt takes a while, so the client polls for the result
    (future (read-receipt ingestion (system/component req :receipt-reader)))
    (api/creation-response ingestion)))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (some-> params
          (select-keys [:id])
          (+scope :receipt-ingestion authenticated ::auth/manage)
          entities/find-by
          (authorize action authenticated)))

(defn- show
  [req]
  (or (some-> (find-and-auth req ::auth/show)
              api/response)
      api/not-found))

(defn- reject
  "Rejects the transaction created from the receipt, deleting it and
  recording the reason on the receipt-ingestion."
  [{{:receipt-ingestion/keys [transaction] :as ingestion} :ingestion
    {:receipt-ingestion/keys [rejection-reason]} :body-params}]
  ; saving first, so the transaction is kept if the rejection is invalid
  (let [result (entities/put (assoc ingestion
                                    :receipt-ingestion/status :rejected
                                    :receipt-ingestion/rejection-reason rejection-reason))]
    (some-> transaction
            entities/find
            entities/delete)
    (dissoc result :receipt-ingestion/transaction)))

(defn- can-reject?
  [{:receipt-ingestion/keys [status transaction]}]
  (and (= :complete status)
       (= :pending (:transaction/review-status
                     (entities/find transaction)))))

(defn- update
  [{:keys [body-params] :as req}]
  (if-let [ingestion (find-and-auth req ::auth/update)]
    (if (= :rejected (:receipt-ingestion/status body-params))
      (if (can-reject? ingestion)
        (api/update-response (reject (assoc req :ingestion ingestion)))
        (api/response {:message "Only ingestions awaiting review can be rejected"} 400))
      (api/response {:message "Only rejecting the transaction is supported"} 400))
    api/not-found))

(def routes
  [["entities/:entity-id/receipt-ingestions" {:post {:handler create}}]
   ["receipt-ingestions/:id" {:get {:handler show}
                              :patch {:handler update}}]])
