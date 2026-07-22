(ns clj-money.cached-accounts
  (:require [cljs.pprint :refer [pprint]]
            [clj-money.util :as util]
            [clj-money.state :as state]
            [clj-money.accounts :refer [nest unnest]]
            [clj-money.api.accounts :as accts]))

(defn fetch-accounts
  [& {:keys [post-xf]}]
  (accts/select
    {}
    :on-success #(reset! state/accounts (->> % nest unnest (into [])))
    :post-xf (or post-xf
                 (map identity))))

(defn latest
  [{:keys [id]}]
  (@state/accounts-by-id id))

(defn watch-entity
  [_ _ previous current]
  (when-not (util/id= previous current)
    (reset! state/accounts nil))
  (when current
    (fetch-accounts)))
