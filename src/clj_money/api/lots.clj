(ns clj-money.api.lots
  (:require [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.api :as api]
            [clj-money.util :as util]
            [clj-money.models :as models]
            [clj-money.authorization :refer [+scope]]
            [clj-money.authorization.lots]))

(defn- extract-criteria
  [{:keys [params authenticated]}]
  (-> (:criteria params)
      (merge (dissoc params :criteria))
      (select-keys [:account-id :commodity-id :shares-owned])
      (update-in-if [:account-id] (fn [x]
                                    (if (sequential? x)
                                      [:in x] ; TODO: should these be model refs also?
                                      (util/->model-ref x))))
      (update-in-if [:commodity-id] util/->model-ref)
      (update-in-if [:shares-owned 0] keyword)
      (update-in-if [:shares-owned 1] bigdec)
      (util/qualify-keys :lot)
      (+scope :lot authenticated)))

(defn index
  [req]
  (-> req
      extract-criteria
      (models/select {:sort [[:lot/purchase-date :asc]]})
      api/response))

(def routes
  [["accounts/:account-id/lots" {:get {:handler index}}]
   ["lots" {:get {:handler index}}]])
