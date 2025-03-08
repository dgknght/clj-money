(ns clj-money.api.lots
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.api-async :as api]
            [clj-money.api :refer [add-error-handler]]))

(defn- prepare-criteria
  [criteria]
  (update-in-if criteria [:shares-owned] (fn [v]
                                           (if (and (sequential? v)
                                                    (= :!= (first v)))
                                             (update-in v [0] name)
                                             v))))

(defn select
  [criteria & {:as opts}]
  {:pre (contains? criteria :account-id)}

  (let [[path criteria] (if (coll? (:account-id criteria))
                          [(api/path :lots)
                           criteria]
                          [(api/path :accounts
                                     (:account-id criteria)
                                     :lots)
                           (dissoc criteria :account-id)])]
    (api/get path
             (prepare-criteria criteria)
             (add-error-handler opts "Unable to retrieve the lots: %s"))))
