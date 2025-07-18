(ns clj-money.cached-accounts
  (:require [cljs.pprint :refer [pprint]]
            [clj-money.util :as util]
            [clj-money.state :refer [accounts]]
            [clj-money.accounts :refer [nest unnest]]
            [clj-money.api.accounts :as accts]))

(defn fetch-accounts
  [& {:keys [post-xf]}]
  (accts/select
    {}
    :on-success #(reset! accounts (->> % nest unnest (into [])))
    :post-xf (or post-xf
                 (map identity))))

(defn watch-entity
  [_ _ previous current]
  (when-not (util/id= previous current)
    (reset! accounts nil))
  (when current
    (fetch-accounts)))
