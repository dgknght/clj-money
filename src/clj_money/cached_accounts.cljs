(ns clj-money.cached-accounts
  (:require [cljs.pprint :refer [pprint]]
            [clj-money.state :refer [accounts]]
            [clj-money.accounts :refer [nest unnest]]
            [clj-money.api.accounts :as accts]))

(defn- reset-accounts
  [retrieved]
  (reset! accounts (->> retrieved nest unnest (into []))))

(defn fetch-accounts
  [& {:keys [post-xf]}]
  (accts/select
    {}
    :on-success reset-accounts
    :post-xf post-xf))

(defn watch-entity
  [_ _ _ current]
  (reset! accounts nil)
  (when current
    (fetch-accounts)))
