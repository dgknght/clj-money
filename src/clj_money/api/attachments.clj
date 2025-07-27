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
            [clj-money.util :as util]
            [clj-money.dates :as dates]
            [clj-money.comparatives :as comparatives :refer [nominative-variations] ]
            [clj-money.io :refer [read-bytes]]
            [clj-money.models :as models]
            [clj-money.models.images :as img]
            [clj-money.authorization.attachments]))

(defn- unserialize-transaction-date
  [criteria]
  (reduce #(update-in-if %1 [%2] dates/unserialize-local-date)
          criteria
          (nominative-variations :transaction-date)))

; TODO: Make this more universal
; The problem is that the framework parses smaller id values
; but not the id values from datomic. We really shouldn't know
; about the storage strategy here. For now, we're just assuming
; that a string that looks like an integer should be one.
(def ^:private long-pattern #"\A\d+\z")
(def ^:private uuid-pattern #"\A[a-f0-9]{8}(-[a-f0-9]{4}){3}-[a-f0-9]{12}\z")

(defn- coerce-id
  [id]
  (if (coll? id)
    (map coerce-id id)
    (if (string? id)
      (cond
        (re-find long-pattern id) (parse-long id)
        (re-find uuid-pattern id) (uuid id))
      id)))

(defn- extract-criteria
  [{:keys [authenticated] {:keys [transaction-id :transaction-date]} :params}]
  (+scope
    {:attachment/transaction
     {:id (coerce-id transaction-id)
      :transaction/transaction-date (dates/unserialize-local-date transaction-date)}}
    :attachment
    authenticated))

(defn- index
  [req]
  (api/response
    (models/select (extract-criteria req))))

(defn- extract-attachment
  [{{:keys [transaction-id transaction-date] :as params} :params}]
  (-> params
      (select-keys [:caption])
      (util/qualify-keys :attachment)
      (merge {:attachment/transaction
              {:id (coerce-id transaction-id)
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
   ["attachments"
    ["" {:get {:handler index}}]
    ["/:id" {:patch {:handler update}
             :delete {:handler delete}}]]])
