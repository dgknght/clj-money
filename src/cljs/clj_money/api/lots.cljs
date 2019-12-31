(ns clj-money.api.lots
  (:require [clj-money.api :as api]
            [clj-money.x-platform.util :refer [unserialize-date]]))

(defn- after-read
  [lot]
  (update-in lot [:purchase-date] unserialize-date))

(defn search
  [criteria success-fn error-fn]
  {:pre (contains? criteria :account-id)}

  (api/get-resources (api/path :accounts
                               (:account-id criteria)
                               :lots)
                     (dissoc criteria :account-id)
                     (comp success-fn
                           #(map after-read %))
                     error-fn))
