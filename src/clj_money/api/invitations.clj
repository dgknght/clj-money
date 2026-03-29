(ns clj-money.api.invitations
  (:require [java-time.api :as t]
            [dgknght.app-lib.api :as api]
            [clj-money.config :refer [env]]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.authorization :refer [authorize +scope]
             :as authorization]
            [clj-money.authorization.invitations]
            [clj-money.entities.invitations :as invitations]
            [clj-money.mailers :as mailers]
            [clj-money.web.auth :refer [make-token]]))

(defn- expiration-instant []
  (t/plus (t/instant)
          (t/days (or (env :invitation-expiration-days) 10))))

(defn- extract-invitation
  [{:keys [params authenticated]}]
  (-> params
      (select-keys [:invitation/recipient
                    :invitation/note])
      (assoc :invitation/invited-by authenticated
             :invitation/status :unsent
             :invitation/expires-at (expiration-instant))))

(defn- send-invitation
  [inv]
  (mailers/send-invitation inv)
  inv)

(defn- create
  [{:keys [params] :as req}]
  (let [inv (-> req
                extract-invitation
                (assoc :invitation/token (invitations/generate-token))
                entities/put)]
    (api/response
      (if (= :sent (:invitation/status params))
        (-> inv
            send-invitation
            (assoc :invitation/status :sent
                   :invitation/expires-at (expiration-instant))
            entities/put)
        inv)
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
  (if-let [inv (find-and-authorize req ::authorization/update)]
    (if (= :unsent (:invitation/status inv))
      (-> inv
          (merge (select-keys (:params req)
                               [:invitation/recipient
                                :invitation/note]))
          entities/put
          api/response)
      api/unprocessable)
    api/not-found))

(defn- delete
  [req]
  (if-let [inv (find-and-authorize req ::authorization/destroy)]
    (if (= :unsent (:invitation/status inv))
      (do
        (entities/delete inv)
        (api/response))
      api/unprocessable)
    api/not-found))

(defn- send-existing
  [{:keys [authenticated] :as req}]
  (if-let [inv (find-and-authorize req ::authorization/update)]
    (if (= :unsent (:invitation/status inv))
      (do
        (mailers/send-invitation (assoc inv :invitation/invited-by authenticated))
        (api/response (-> inv
                          (assoc :invitation/status :sent
                                 :invitation/expires-at (expiration-instant))
                          entities/put)))
      api/unprocessable)
    api/not-found))

(def ^:private invitation-expired
  (api/response {:message "invitation expired"} 410))

(defn- expired?
  [{:invitation/keys [expires-at]}]
  (and expires-at
       (t/before? (t/instant expires-at) (t/instant))))

(defn- find-by-token
  [{:keys [params]}]
  (if-let [inv (entities/find-by {:invitation/token (:token params)})]
    (if (expired? inv)
      invitation-expired
      (api/response inv))
    api/not-found))

(defn- accept
  [{:keys [params]}]
  (if-let [inv (entities/find-by {:invitation/token (:token params)})]
    (if (expired? inv)
      invitation-expired
      (let [user (-> params
                     (select-keys [:user/first-name
                                   :user/last-name
                                   :user/password])
                     (assoc :user/email (:invitation/recipient inv)
                            :user/roles #{:user})
                     entities/put)]
        (-> inv
            (assoc :invitation/status :accepted
                   :invitation/user user)
            entities/put)
        (api/creation-response {:user user
                                :auth-token (make-token user)})))
    api/not-found))

(defn- decline
  [{:keys [params]}]
  (if-let [inv (entities/find-by {:invitation/token (:token params)})]
    (if (expired? inv)
      invitation-expired
      (do
        (-> inv
            (assoc :invitation/status :declined)
            entities/put)
        (api/response)))
    api/not-found))

(def routes
  [["invitations"
    ["" {:get {:handler index}
         :post {:handler create}}]
    ["/:id"
     ["" {:get {:handler show}
          :patch {:handler patch}
          :delete {:handler delete}}]
     ["/send" {:post {:handler send-existing}}]]]])

(def unauthenticated-routes
  [["invitations/:token"
    ["/accept" {:get {:handler find-by-token}
                :post {:handler accept}}]
    ["/decline" {:post {:handler decline}}]]])
