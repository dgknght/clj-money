(ns clj-money.api.attachments
  (:refer-clojure :exclude [update find])
  (:require [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [uuid
                                          update-in-if]]
            [dgknght.app-lib.api :as api]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [clj-money.dates :as dates]
            [clj-money.util :as util :refer [nominative-variations
                                             symbolic-comparatives]]
            [clj-money.io :refer [read-bytes]]
            [clj-money.models :as models]
            [clj-money.models.images :as img]
            [clj-money.authorization.attachments]))

(defn- unserialize-transaction-date
  [criteria]
  (reduce #(update-in-if %1 [%2] dates/unserialize-local-date)
          criteria
          (nominative-variations :transaction-date)))

(defn- extract-criteria
  [{:keys [params authenticated]}]
  (-> params
      unserialize-transaction-date
      (symbolic-comparatives :transaction-date)
      (rename-keys {"transaction-id[]" :transaction/id
                    :transaction-id :transaction/id
                    :transaction-date :transaction/transaction-date})
      (update-in-if [:transaction/id] #(if (coll? %)
                                         (map uuid %)
                                         (uuid %)))
      (+scope :attachment authenticated)))

(defn- index
  [req]
  (api/response
    (models/select (extract-criteria req))))

(defn- extract-attachment
  [{:keys [params]}]
  (-> params
      (select-keys [:transaction-id :transaction-date])
      (util/qualify-keys :attachment)
      (update-in [:attachment/transaction-id] (comp util/->model-ref
                                                    uuid))
      (update-in [:attachment/transaction-date] dates/unserialize-local-date)
      (rename-keys {:attachment/transaction-id :attachment/transaction})))

(defn- find-or-create-image
  [{{:keys [file]} :params
    :keys [authenticated]}]
  (-> file
      (select-keys [:content-type :filename :tempfile])
      (update-in [:tempfile] read-bytes)
      (rename-keys {:filename :image/original-filename
                    :tempfile :image/body
                    :content-type :image/content-type})
      (assoc :image/user authenticated)
      img/find-or-create))

(defn- assoc-image
  [att req]
  (if-let [image (find-or-create-image req)]
    (assoc att :attachment/image image)
    att))

(defn- create
  [{:keys [authenticated] :as req}]
  (-> (extract-attachment req)
      (authorize ::auth/create authenticated)
      (assoc-image req)
      models/put
      api/creation-response))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (when-let [attachment (models/find-by (-> params
                                            (select-keys [:id])
                                            (+scope :attachment authenticated)))]
    (authorize
     attachment
     action
     authenticated)))

(defn- update
  [{:keys [body] :as req}]
  (if-let [attachment (find-and-auth req ::auth/update)]
    (-> attachment
        (merge (select-keys body [:caption]))
        models/put
        api/update-response)
    api/not-found))

(defn- delete
  [req]
  (if-let [attachment (find-and-auth req ::auth/destroy)]
    (do
      (models/delete attachment)
      (api/response))
    api/not-found))

(def routes
  [["transactions/:transaction-id/:transaction-date/attachments" {:post {:handler create}}]
   ["attachments"
    ["" {:get {:handler index}}]
    ["/:id" {:patch {:handler update}
             :delete {:handler delete}}]]])
