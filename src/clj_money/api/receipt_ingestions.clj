(ns clj-money.api.receipt-ingestions
  (:refer-clojure :exclude [update])
  (:require [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [dgknght.app-lib.api :as api]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [clj-money.util :as util]
            [clj-money.io :refer [read-bytes]]
            [clj-money.images :as images]
            [clj-money.entities :as entities]
            [clj-money.entities.images :as img]
            [clj-money.entities.propagation :as prop]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.receipts :as rcpts]
            [clj-money.receipts :as receipts]
            [clj-money.web.system :as system]
            [clj-money.authorization.receipt-ingestions]))

(defn- simplify-accounts
  "Refers to the accounts in the receipt by id"
  [receipt]
  (-> receipt
      (update-in [:receipt/payment-account] util/->entity-ref)
      (update-in [:receipt/items]
                 (partial mapv #(update-in % [:receipt-item/account] util/->entity-ref)))))

(defn- create-transaction
  "Creates the transaction described by the data read from the receipt
  image. It awaits review by the user."
  [data entity]
  (-> (rcpts/make-trx data entity)
      (assoc :transaction/entity entity
             :transaction/source :ingestion
             :transaction/review-status :pending)
      prop/put-and-propagate))

(defn- read-receipt
  [ingestion reader]
  (let [{:receipt-ingestion/keys [entity image] :as ingestion}
        (entities/put (assoc ingestion :receipt-ingestion/status :processing))
        entity (entities/find entity)]
    (try
      (let [trx (-> (ing/read-receipt reader
                                      (images/get (:image/uuid (entities/find image)))
                                      entity
                                      {})
                    (create-transaction entity))]
        (entities/put (assoc ingestion
                             :receipt-ingestion/status :complete
                             :receipt-ingestion/transaction (util/->entity-ref trx)
                             :receipt-ingestion/receipt (-> trx
                                                            receipts/<-transaction
                                                            simplify-accounts
                                                            util/remove-nils))))
      (catch Exception e
        (log/errorf e "[receipt-ingestion] unable to read receipt %s" (:id ingestion))
        (entities/put (assoc ingestion
                             :receipt-ingestion/status :failed
                             :receipt-ingestion/error (ex-message e)))))))

(defn- create-image
  [{{:keys [image]} :params
    :keys [authenticated]}]
  (-> image
      (select-keys [:content-type :filename :tempfile])
      (update-in [:tempfile] read-bytes)
      (rename-keys {:filename :image/original-filename
                    :tempfile :image/content
                    :content-type :image/content-type})
      (assoc :image/user authenticated)
      img/find-or-create
      util/->entity-ref))

(defn- create
  [{:keys [authenticated] {:keys [entity-id]} :params :as req}]
  (let [ingestion (-> #:receipt-ingestion{:entity {:id entity-id}
                                          :status :pending}
                      (authorize ::auth/create authenticated)
                      (assoc :receipt-ingestion/image (create-image req))
                      entities/put)]
    ; reading the receipt takes a while, so the client polls for the result
    (future (read-receipt ingestion (system/component req :ingestion)))
    (api/creation-response ingestion)))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (some-> params
          (select-keys [:id])
          (+scope :receipt-ingestion authenticated)
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
            prop/delete-and-propagate)
    (dissoc result :receipt-ingestion/transaction)))

(defn- update
  [{:keys [body-params] :as req}]
  (if-let [ingestion (find-and-auth req ::auth/update)]
    (if (= :rejected (util/ensure-keyword (:receipt-ingestion/status body-params)))
      (api/update-response (reject (assoc req :ingestion ingestion)))
      (api/response {:message "Only rejecting the transaction is supported"} 400))
    api/not-found))

(def routes
  [["entities/:entity-id/receipt-ingestions" {:post {:handler create}}]
   ["receipt-ingestions/:id" {:get {:handler show}
                              :patch {:handler update}}]])
