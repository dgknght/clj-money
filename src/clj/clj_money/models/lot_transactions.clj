(ns clj-money.models.lot-transactions
  (:refer-clojure :exclude [update find count])
  (:require [config.core :refer [env]]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage]]
            [clj-money.models :as models]))

(defn- after-read
  [lot-transaction]
  (-> lot-transaction
      (update-in [:lot-action] keyword)
      (tag ::models/lot-transaction)))

(defn search
  ([criteria]
   (search criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (map after-read (storage/select (tag criteria ::models/lot-transaction) options)))))

(defn count
  ([criteria] (count criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (->> (storage/select
            (tag criteria ::models/lot-transaction)
            (assoc options :count true))
          (map :record-count)
          first))))

(defn find-by
  ([criteria] (find-by criteria {}))
  ([criteria options]
   (first (search criteria (assoc options :limit 1)))))

(defn create
  [lot-transaction]
  {:pre [(:transaction-id lot-transaction)
         (:transaction-date lot-transaction)]}

  (with-storage (env :db)
    (after-read
     (storage/create (tag lot-transaction
                          ::models/lot-transaction)))))

(defn update
  [lot-transaction]
  (with-storage (env :db)
    (storage/update lot-transaction))
  (find-by (select-keys lot-transaction [:lot-id :transaction-id])))

(defn delete
  [lot-transaction]
  (with-storage (env :db)
    (storage/delete (tag lot-transaction ::models/lot-transaction))))
