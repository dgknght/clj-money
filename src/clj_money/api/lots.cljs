(ns clj-money.api.lots
  (:require [cljs.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn- prepare-criteria
  [{:lot/keys [shares-owned] :as criteria}]
  (cond-> (-> criteria
              (dissoc :lot/shares-owned)
              (update-in-if [:lot/account] #(map :id %))
              (update-in-if [:lot/commodity] :id)
              (rename-keys {:lot/account :account-id
                            :lot/commodity :commodity-id}))
    (= [:!= 0M] shares-owned) (assoc :non-zero-shares true)))

(defn select
  [{:lot/keys [account] :as criteria} & {:as opts}]
  {:pre (contains? criteria :lot/account)}

  (let [[path c] (if (sequential? account)
                   [(api/path :lots)
                    criteria]
                   [(api/path :accounts
                              account
                              :lots)
                    (dissoc criteria :lot/account)])]
    (api/get path
             (prepare-criteria c)
             (add-error-handler opts "Unable to retrieve the lots: %s"))))
