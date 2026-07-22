(ns clj-money.db.datomic.accounts
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [difference]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.entities :as ents]
            [clj-money.dates :as dates]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/deconstruct :account
  [{:account/keys [allocations] :as account}]
  ; TODO: Refine this to handle removing allocations that have been changed or removed
  (let [removed-user-tags (difference (:account/user-tags (ents/before account))
                                      (:account/user-tags account))
        removed-allocations (difference
                              (-> account ents/before :account/allocations set)
                              (-> account :account/allocations set))]
    (concat [(dissoc account :account/allocations)]
            (map (fn [[k v]]
                   [:db/add
                    (:id account)
                    :account/allocations
                    [k v]])
                 allocations)
            (map (fn [a]
                   [:db/retract (:id account) :account/allocations a])
                 removed-allocations)
            (map (fn [t]
                   [:db/retract (:id account) :account/user-tags t])
                 removed-user-tags))))

(defmethod datomic/after-read :account
  [account]
  (update-in-if account [:account/price-as-of] dates/->local-date))
