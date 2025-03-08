(ns clj-money.cached-accounts
  (:require [clj-money.state :refer [accounts]]
            [clj-money.accounts :refer [nest unnest]]
            [clj-money.api.accounts :as accts]))

(defn- reset-accounts
  [retrieved]
  (reset! accounts (->> retrieved nest unnest (into []))))

(defn fetch-accounts
  ([]
   (accts/select {} :on-success reset-accounts))
  ([xf]
   (accts/select {}
                 :on-success reset-accounts
                 :post-xf xf)))

(defn watch-entity
  [_ _ _ current]
  (reset! accounts nil)
  (when current
    (fetch-accounts)))
