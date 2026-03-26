(ns clj-money.api.invitations
  (:require [dgknght.app-lib.api :as api]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.authorization :refer [authorize +scope]
             :as authorization]
            [clj-money.authorization.invitations]))

(defn- extract-invitation
  [{:keys [params authenticated]}]
  (-> params
      (select-keys [:invitation/recipient
                    :invitation/note
                    :invitation/status])
      (assoc :invitation/user authenticated)))

(defn- create
  [req]
  (-> req
      extract-invitation
      entities/put
      (api/response 201)))

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

(def routes
  [["invitations"
    ["" {:get {:handler index}
         :post {:handler create}}]
    ["/:id" {:get {:handler show}
             :patch {:handler patch}
             :delete {:handler delete}}]]])
