(ns clj-money.api.lots
  (:require [clj-money.api :as api]
            [clj-money.util :refer [unserialize-date]]))

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
    (api/get-resources path
                       criteria
                       (comp success-fn
                             #(map after-read %))
                       error-fn)))
