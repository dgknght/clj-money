(ns clj-money.db.datomic.accounts
  (:require [clojure.pprint :refer [pprint]]
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
