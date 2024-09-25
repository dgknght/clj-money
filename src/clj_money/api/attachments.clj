(ns clj-money.api.attachments
  (:refer-clojure :exclude [update find])
  (:require [clojure.set :refer [rename-keys]]
            [stowaway.core :refer [tag]]
            [dgknght.app-lib.core :refer [uuid
                                          update-in-if]]
            [dgknght.app-lib.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [dgknght.app-lib.api :as api]
            [clj-money.dates :as dates]
            [clj-money.util :refer [nominative-variations
                                    symbolic-comparatives]]
            [clj-money.io :refer [read-bytes]]
            [clj-money.models :as models]
            [clj-money.models.images :as img]
            [clj-money.models.attachments :as att]
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
      (rename-keys {"transaction-id[]" :transaction-id})
      (update-in-if [:transaction-id] #(if (coll? %)
                                         (map uuid %)
                                         (uuid %)))
      (+scope ::models/attachment authenticated)))

(defn- index
  [req]
  (api/response
    (att/search (extract-criteria req))))

(defn- extract-attachment
  [{:keys [params]}]
  (-> params
      (select-keys [:transaction-id :transaction-date])
      (update-in [:transaction-id] uuid)
      (update-in [:transaction-date] dates/unserialize-local-date)
      (tag ::models/attachment)))

(defn- create-image
  [{{:keys [file]} :params
    :keys [authenticated]}]
  (-> file
      (select-keys [:content-type :filename :tempfile])
      (update-in [:tempfile] read-bytes)
      (rename-keys {:filename :original-filename
                    :tempfile :body})
      (assoc :user-id (:id authenticated))
      img/find-or-create))

(defn- assoc-image
  [att req]
  (if-let [image (create-image req)]
    (assoc att :image-id (:id image))
    att))

(defn- create
  [{:keys [authenticated] :as req}]
  (-> (extract-attachment req)
      (authorize ::auth/create authenticated)
      (assoc-image req)
      att/create
      api/creation-response))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (when-let [attachment (att/find-by (-> params
                                         (select-keys [:id])
                                         (+scope ::models/attachment authenticated)))]
    (authorize
     attachment
     action
     authenticated)))

(defn- update
  [{:keys [body] :as req}]
  (if-let [attachment (find-and-auth req ::auth/update)]
    (-> attachment
        (merge (select-keys body [:caption]))
        att/update
        api/update-response)
    api/not-found))

(defn- delete
  [req]
  (if-let [attachment (find-and-auth req ::auth/destroy)]
    (do
      (att/delete attachment)
      (api/response))
    api/not-found))

(def routes
  [["transactions/:transaction-id/:transaction-date/attachments" {:post {:handler create}}]
   ["attachments"
    ["" {:get {:handler index}}]
    ["/:id" {:patch {:handler update}
             :delete {:handler delete}}]]])
