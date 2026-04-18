(ns clj-money.api.audit
  (:require [clojure.set :refer [rename-keys]]
            [dgknght.app-lib.api :as api]
            [clj-money.db :as db :refer [unserialize-id]]
            [clj-money.authorization :refer [+scope authorize]
             :as authorization]
            [clj-money.entities :as entities]
            [clj-money.authorization.lots]
            [clj-money.authorization.transaction-items]))

(defn- find-and-auth
  [path-params id-key entity-type authenticated]
  (some-> path-params
          (select-keys [id-key])
          (update-in [id-key] unserialize-id)
          (rename-keys {id-key :id})
          (+scope entity-type authenticated)
          entities/find-by
          (authorize ::authorization/show authenticated)))

(defn- index
  [id-key entity-type]
  (fn [{:keys [path-params params authenticated]}]
    (if-let [entity (find-and-auth path-params id-key entity-type authenticated)]
      (api/response
        (db/history (db/storage)
                    (:id entity)
                    (keyword (:attr params))))
      api/not-found)))

(def routes
  [["lots/:lot-id/audit"
    {:get {:handler (index :lot-id :lot)}}]
   ["transaction-items/:transaction-item-id/audit"
    {:get {:handler (index :transaction-item-id :transaction-item)}}]])
