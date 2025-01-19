(ns clj-money.cached-accounts
  (:require [clj-money.state :refer [accounts]]
            [clj-money.accounts :refer [nest unnest]]
            [clj-money.api.accounts :as accts]))

(defn- reset-accounts
  [retrieved]
  (reset! accounts (->> retrieved nest unnest (into []))))

(defn fetch-accounts
  ([] (fetch-accounts nil))
  ([xf]
   (let [xform (cond-> (map reset-accounts)
                 xf (comp xf))]
     (accts/select xform))))

(defn watch-entity
  [_ _ _ current]
  (reset! accounts nil)
  (when current
    (fetch-accounts)))
