(ns clj-money.db.datomic.scheduled-transactions
  (:require [clj-money.db.datomic :as datomic]
            [clj-money.entities :as entities]))

(defmethod datomic/before-save :scheduled-transaction
  [trx]
  (update-in trx [:scheduled-transaction/date-spec] pr-str))

(defmethod datomic/deconstruct :scheduled-transaction
  [trx]
  (if-let [before (::entities/before (meta trx))]
    (let [after-ids (->> (:scheduled-transaction/items trx)
                         (map :id)
                         set)]
      (cons trx
            (map (fn [item] [:db/retractEntity (:id item)])
                 (remove (comp after-ids :id)
                         (:scheduled-transaction/items before)))))
    [trx]))
