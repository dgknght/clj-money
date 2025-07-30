(ns clj-money.api.attachments
  (:refer-clojure :exclude [update find])
  (:require [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.api :as api]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [clj-money.util :as util]
            [clj-money.dates :as dates]
            [clj-money.io :refer [read-bytes]]
            [clj-money.models :as models]
            [clj-money.models.images :as img]
            [clj-money.authorization.attachments]))

(defn- extract-criteria
  [{:keys [authenticated] {:keys [transaction-id :transaction-date]} :params}]
  (+scope
    {:attachment/transaction
     {:id (util/coerce-id transaction-id)
      :transaction/transaction-date (dates/unserialize-local-date transaction-date)}}
    :attachment
    authenticated))

(defn- index
  [req]
  (api/response
    (models/select (extract-criteria req))))

(defn- extract-account-criteria
  [{:keys [authenticated] {:keys [start-date end-date account-id]} :params}]
  (+scope
    {:transaction-item/account {:id account-id}
     :transaction/transaction-date
     [:between>
      (dates/unserialize-local-date start-date)
      (dates/unserialize-local-date end-date)]}
    :attachment
    authenticated))

(defn- index-by-account
  [req]
  (api/response
    (models/select (extract-account-criteria req))))

(defn- extract-attachment
  [{{:keys [transaction-id transaction-date] :as params} :params}]
  (-> params
      (select-keys [:caption])
      (util/qualify-keys :attachment)
      (merge {:attachment/transaction
              {:id (util/coerce-id transaction-id)
               :transaction/transaction-date (dates/unserialize-local-date transaction-date)}})))

(defn- find-or-create-image
  [{{:keys [file]} :params
    :keys [authenticated]}]
  (-> file
      (select-keys [:content-type :filename :tempfile])
      (update-in [:tempfile] read-bytes)
      (rename-keys {:filename :image/original-filename
                    :tempfile :image/content
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
  [{:keys [body-params] :as req}]
  (if-let [attachment (find-and-auth req ::auth/update)]
    (-> attachment
        (merge (select-keys body-params [:attachment/caption]))
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
  [["transactions/:transaction-id/:transaction-date/attachments" {:post {:handler create}
                                                                  :get {:handler index}}]
   ["accounts/:account-id/attachments/:start-date/:end-date" {:get {:handler index-by-account}}]
   ["attachments"
    ["" {:get {:handler index}}]
    ["/:id" {:patch {:handler update}
             :delete {:handler delete}}]]])
