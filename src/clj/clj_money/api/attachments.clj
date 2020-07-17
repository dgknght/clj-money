(ns clj-money.api.attachments
  (:refer-clojure :exclude [update find])
  (:require [clojure.set :refer [rename-keys]]
            [environ.core :refer [env]]
            [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [stowaway.core :as stow]
            [clj-money.x-platform.util :refer [update-in-if
                                               unserialize-date
                                               nominative-variations
                                               symbolic-comparatives]]
            [clj-money.io :refer [read-bytes]]
            [clj-money.util :refer [uuid]]
            [clj-money.api :refer [->response
                                   not-found
                                   bad-request]]
            [clj-money.models :as models]
            [clj-money.validation :as v]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [clj-money.models.images :as img]
            [clj-money.models.attachments :as att]
            [clj-money.authorization.attachments]))

(defn- unserialize-transaction-date
  [criteria]
  (reduce #(update-in-if %1 [%2] unserialize-date)
          criteria
          (nominative-variations :transaction-date)))

(defn- extract-criteria
  [{:keys [params authenticated]}]
  (-> params
      unserialize-transaction-date
      (symbolic-comparatives :transaction-date)
      (rename-keys {"transaction-id[]" :transaction-id})
      (update-in-if [:transaction-id] #(if (vector? %)
                                         (map uuid %)
                                         (uuid %)))
      (+scope ::models/attachment authenticated)))

(defn- index
  [req]
  (->response (att/search (env :db)
                          (extract-criteria req))))

(defn- create
  [{:keys [params authenticated]}]
  (let [transaction-id (uuid (:transaction-id params))
        transaction-date (unserialize-date (:transaction-date params))
        attr {:transaction-id transaction-id
              :transaction-date transaction-date}]
    (authorize (stow/tag attr ::models/attachment)
               ::auth/create
               authenticated)
    (let [image (img/find-or-create (env :db)
                                    {:user-id (:id authenticated)
                                     :content-type (:content-type (:file params))
                                     :original-filename (:filename (:file params))
                                     :body (read-bytes (:tempfile (:file params)))})
          attachment (when (:id image)
                       (att/create (env :db)
                                   (assoc attr :image-id (:id image))))]
      (if attachment
        (->response attachment
                    (if (v/has-error? attachment)
                      400
                      201))
        (bad-request)))))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (when-let [attachment (att/find-by (env :db)
                                     (-> params
                                         (select-keys [:id])
                                         (+scope ::models/attachment authenticated)))]
    (authorize
      attachment
      action
      authenticated)))

(defn- update
  [{:keys [body] :as req}]
  (if-let [attachment (find-and-auth req ::auth/update)]
    (->response
      (att/update (env :db)
                  (merge attachment
                         (select-keys body [:caption]))))
    (not-found)))

(defn- delete
  [req]
  (if-let [attachment (find-and-auth req ::auth/destroy)]
    (do
      (att/delete (env :db) attachment)
      (->response))
    (not-found)))

(defroutes routes
  (POST "/api/transactions/:transaction-id/:transaction-date/attachments" req (create req))
  (GET "/api/attachments" req (index req))
  (PATCH "/api/attachments/:id" req (update req))
  (DELETE "/api/attachments/:id" req (delete req)))
