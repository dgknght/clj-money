(ns clj-money.api.audit
  (:require [dgknght.app-lib.api :as api]
            [clj-money.db :as db :refer [unserialize-id]]
            [clj-money.authorization :refer [authorize]
             :as authorization]
            [clj-money.entities :as entities]
            [clj-money.authorization.lots]
            [clj-money.authorization.transaction-items]))

(defn- find-and-auth
  [{:keys [path-params authenticated]}]
  (some-> path-params
          :id
          unserialize-id
          entities/find
          (authorize ::authorization/show authenticated)))

(defn- index
  [{:keys [params] :as req}]
  (if-let [entity (find-and-auth req)]
    (api/response
      (db/history (db/storage)
                  (:id entity)
                  (keyword (:attr params))))
    api/not-found))

(def routes
  ["entities/:id/audit"
   {:get {:handler index}}])
