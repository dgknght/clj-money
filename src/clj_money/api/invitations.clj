(ns clj-money.api.invitations
  (:require [dgknght.app-lib.api :as api]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.authorization :refer [authorize +scope]
             :as authorization]
            [clj-money.authorization.invitations]
            [clj-money.entities.invitations :as invitations]
            [clj-money.mailers :as mailers]
            [clj-money.web.auth :refer [make-token]]))

(defn- extract-invitation
  [{:keys [params authenticated]}]
  (-> params
      (select-keys [:invitation/recipient
                    :invitation/note
                    :invitation/status])
      (assoc :invitation/user authenticated)))

(defn- create
  [{:keys [authenticated] :as req}]
  (let [inv (-> req
                extract-invitation
                (assoc :invitation/token (invitations/generate-token))
                entities/put)]
    (mailers/send-invitation (assoc inv :invitation/user authenticated))
    (api/response (-> inv
                      (assoc :invitation/status :sent)
                      entities/put)
                  201)))

(defn- find-and-authorize
  [{:keys [params authenticated]} action]
  (some-> params
          (select-keys [:id])
          (util/entity-type :invitation)
          (+scope :invitation authenticated)
          entities/find-by
          (authorize action authenticated)))

(defn- index
  [{:keys [authenticated]}]
  (api/response
    (entities/select (+scope {} :invitation authenticated))))

(defn- show
  [req]
  (or (some-> (find-and-authorize req ::authorization/show)
              api/response)
      api/not-found))

(defn- patch
  [req]
  (or (some-> (find-and-authorize req ::authorization/update)
              (merge (select-keys (:params req)
                                  [:invitation/status
                                   :invitation/note]))
              entities/put
              api/response)
      api/not-found))

(defn- delete
  [req]
  (if-let [inv (find-and-authorize req ::authorization/destroy)]
    (do
      (entities/delete inv)
      (api/response))
    api/not-found))

(defn- find-by-token
  [{:keys [params]}]
  (if-let [inv (entities/find-by {:invitation/token (:token params)})]
    (api/response inv)
    api/not-found))

(defn- accept
  [{:keys [params]}]
  (if-let [inv (entities/find-by {:invitation/token (:token params)})]
    (let [user (-> params
                   (select-keys [:user/first-name
                                 :user/last-name
                                 :user/password])
                   (assoc :user/email (:invitation/recipient inv)
                          :user/roles #{:user})
                   entities/put)]
      (-> inv
          (assoc :invitation/status :accepted)
          entities/put)
      (api/creation-response {:user user
                              :auth-token (make-token user)}))
    api/not-found))

(def routes
  [["invitations"
    ["" {:get {:handler index}
         :post {:handler create}}]
    ["/:id" {:get {:handler show}
             :patch {:handler patch}
             :delete {:handler delete}}]]])

(def unauthenticated-routes
  [["invitations/accept"
    ["/:token" {:get {:handler find-by-token}}]
    ["" {:post {:handler accept}}]]])
