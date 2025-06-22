(ns clj-money.db.datomic.transactions
  (:require [clj-money.db.datomic :as datomic]))

(defmethod datomic/deconstruct :transaction
  [{:as transaction :transaction/keys [items]}]
  (cons (dissoc transaction :transaction/items)
        items))
