(ns clj-money.api.audit
  (:require [clojure.set :refer [rename-keys]]
            [dgknght.app-lib.api :as api]
            [clj-money.db :as db :refer [unserialize-id]]
            [clj-money.authorization :refer [+scope authorize]
             :as authorization]
            [clj-money.entities :as entities]
            [clj-money.authorization.lots]))

(defn- find-lot-and-auth
  [{:keys [path-params authenticated]}]
  (some-> path-params
          (select-keys [:lot-id])
          (update-in [:lot-id] unserialize-id)
          (rename-keys {:lot-id :id})
          (+scope :lot authenticated)
          entities/find-by
          (authorize ::authorization/show authenticated)))

(defn- lot-audit
  [{:as req :keys [params]}]
  (if-let [lot (find-lot-and-auth req)]
    (api/response
      (db/history (db/storage) (:id lot) (keyword (:attr params))))
    api/not-found))

(def routes
  [["lots/:lot-id/audit" {:get {:handler lot-audit}}]])
