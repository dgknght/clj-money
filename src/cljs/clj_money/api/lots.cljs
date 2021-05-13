(ns clj-money.api.lots
  (:require [dgknght.app-lib.web :refer [unserialize-date]]
            [dgknght.app-lib.api :as api]))

(defn- after-read
  [lot]
  (update-in lot [:purchase-date] unserialize-date))

(defn search
  [criteria success-fn error-fn]
  {:pre (contains? criteria :account-id)}

  (let [[path criteria] (if (coll? (:account-id criteria))
                          [(api/path :lots)
                           criteria]
                          [(api/path :accounts
                                     (:account-id criteria)
                                     :lots)
                           (dissoc criteria :account-id)])]
    (api/get path
             criteria
             (comp success-fn
                   #(map after-read %))
             error-fn)))
