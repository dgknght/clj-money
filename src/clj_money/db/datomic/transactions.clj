(ns clj-money.db.datomic.transactions
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.db.datomic :as datomic]
            [clj-money.entities :as entities]
            [clj-money.util :as util]))

(defn- removed
  [before after]
  (let [after-ids (->> after
                       (map :id)
                       set)]
    (remove (comp after-ids :id)
            before)))

(defn- item-redactions
  [trx before]
  (map (fn [item]
         [:db/retractEntity (:id item)])
       (removed (:transaction/items before)
                (:transaction/items trx))))

(defmethod datomic/deconstruct :transaction
  [trx]
  (if-let [before (::entities/before (meta trx))]
    (cons trx
          (item-redactions trx before))
    [trx]))
