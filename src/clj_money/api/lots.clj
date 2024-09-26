(ns clj-money.api.lots
  (:require [dgknght.app-lib.core :refer [update-in-if
                                     parse-float]]
            [dgknght.app-lib.api :as api]
            [dgknght.app-lib.authorization :refer [+scope]]
            [clj-money.models :as models]
            [clj-money.models.lots :as lots]
            [clj-money.authorization.lots]))

(defn index
  [{:keys [params authenticated]}]
  (api/response
    (lots/search (-> (:criteria params)
                     (merge (dissoc params :criteria))
                     (select-keys [:account-id :commodity-id :shares-owned])
                     (update-in-if [:shares-owned 0] keyword)
                     (update-in-if [:shares-owned 1] parse-float)
                     (+scope ::models/lot authenticated))
                 {:sort [[:purchase-date :asc]]})))

(def routes
  [["accounts/:account-id/lots" {:get {:handler index}}]
   ["lots" {:get {:handler index}}]])
