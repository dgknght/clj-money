(ns clj-money.api.lots
  (:require [compojure.core :refer [defroutes GET]]
            [environ.core :refer [env]]
            [clj-money.api :refer [->response]]
            [clj-money.authorization :refer [+scope]]
            [clj-money.models :as models]
            [clj-money.models.lots :as lots]
            [clj-money.authorization.lots]))

(defn index
  [{:keys [params authenticated]}]
  (->response (lots/search (env :db)
                           (-> (:criteria params)
                               (merge (dissoc params :criteria))
                               (select-keys [:account-id :commodity-id :shares-owned])
                               (+scope ::models/lot authenticated))
                           {:sort [[:purchase-date :asc]]})))

(defroutes routes
  (GET "/api/accounts/:account-id/lots" req (index req)))
