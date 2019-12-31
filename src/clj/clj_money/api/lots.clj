(ns clj-money.api.lots
  (:require [clj-money.api :refer [index-resource]]
            [clj-money.x-platform.util :refer [update-in-criteria]]
            [clj-money.models.lots :as lots]
            [clj-money.permissions.lots]))

(defn- parse-int
  [s]
  (cond
    (and s (string? s) (re-find #"^\d+$" s))
    (Integer/parseInt s)

    (integer? s) s

    :else nil))

(defn index
  [{:keys [params]}]
  (let [criteria (-> (:criteria params)
                     (merge (dissoc params :criteria))
                     (select-keys [:account-id :commodity-id :shares-owned])
                     (update-in-criteria :account-id parse-int)
                     (update-in-criteria :commodity-id parse-int)
                     (update-in-criteria :shares-owned parse-int))]
    (index-resource lots/search criteria :lot)))
