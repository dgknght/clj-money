(ns clj-money.api.lots
  (:require [compojure.core :refer [defroutes GET]]
            [environ.core :refer [env]]
            [clj-money.api :refer [->response]]
            [clj-money.authorization :refer [apply-scope]]
            [clj-money.models.lots :as lots]
            [clj-money.permissions.lots]))

(defn index
  [{:keys [params authenticated]}]
  (->response (lots/search (env :db) (-> (:criteria params)
                                         (merge (dissoc params :criteria))
                                         (select-keys [:account-id :commodity-id :shares-owned])
                                         (apply-scope :lot authenticated)))))

(defroutes routes
  (GET "/api/accounts/:account-id/lots" req (index req)))
