(ns clj-money.db.datomic.accounts
  (:require [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/deconstruct :account
  [{:account/keys [allocations] :as account}]
  ; TODO: Refine this to handle removing allocations that have been changed or removed
  (cons (dissoc account :account/allocations)
        (map (fn [[k v]]
               [:db/add
                (:id account)
                :account/allocations
                [k v]])
             allocations)))

(defmethod datomic/after-read :account
  [account]
  (-> account
      (update-in-if [:account/system-tags] set)
      (update-in-if [:account/user-tags] set)
      (update-in-if [:account/allocations] #(into {} %))))
