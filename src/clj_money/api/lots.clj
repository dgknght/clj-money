(ns clj-money.api.lots
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.api :as api]
            [clj-money.util :as util]
            [clj-money.models :as models]
            [clj-money.authorization :refer [+scope]]
            [clj-money.authorization.lots]))

(defn- extract-criteria
  [{:keys [params]}]
  (cond-> (-> (:criteria params)
              (merge (dissoc params :criteria))
              (select-keys [:account-id :commodity-id])
              (update-in-if [:account-id] (fn [x]
                                            (if (sequential? x)
                                              [:in (map util/->model-ref x)]
                                              (util/->model-ref x))))
              (update-in-if [:commodity-id] util/->model-ref)
              (rename-keys {:account-id :lot/account
                            :commodity-id :lot/commodity}))
    (:non-zero-shares params) (assoc :lot/shares-owned [:!= 0M])))

(defn index
  [{:as req :keys [authenticated]}]
  (-> req
      extract-criteria
      (+scope :lot authenticated)
      (models/select {:sort [[:lot/purchase-date :asc]]})
      api/response))

(def routes
  [["accounts/:account-id/lots" {:get {:handler index}}]
   ["lots" {:get {:handler index}}]])
