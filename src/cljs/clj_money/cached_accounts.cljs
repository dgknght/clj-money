(ns clj-money.cached-accounts
  (:require [dgknght.app-lib.notifications :as notify]
            [clj-money.state :refer [accounts
                                     current-entity]]
            [clj-money.accounts :refer [nest unnest]]
            [clj-money.api.accounts :as accounts]))

(defn fetch-accounts
  ([] (fetch-accounts identity))
  ([callback]
   (accounts/select (fn [result]
                      (reset! accounts (->> result
                                            nest
                                            unnest))
                      (callback))
                    (notify/danger-fn (str "Unable to load the accounts for " (:name @current-entity))))))

(defn watch-entity
  [_ _ _ current]
  (if current
    (do
      (reset! accounts nil)
      (fetch-accounts))
    (reset! accounts nil)))
