(ns clj-money.api.lots
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn- prepare-criteria
  [criteria]
  (update-in-if criteria [:lot/shares-owned] (fn [v]
                                               (if (and (sequential? v)
                                                        (= :!= (first v)))
                                                 (update-in v [0] name)
                                                 v))))

(defn select
  [criteria & {:as opts}]
  {:pre (contains? criteria :lot/account)}

  (let [[path criteria] (if (coll? (:lot/account criteria))
                          [(api/path :lots)
                           criteria]
                          [(api/path :accounts
                                     (:lot/account criteria)
                                     :lots)
                           (dissoc criteria :lot/account)])]
    (api/get path
             (prepare-criteria criteria)
             (add-error-handler opts "Unable to retrieve the lots: %s"))))
