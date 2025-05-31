(ns clj-money.db.sql.scheduled-transactions
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if
                                          parse-int]]
            [clj-money.util :as util]
            [clj-money.db :as db]
            [clj-money.db.sql :as sql]))

(defmethod sql/deconstruct :scheduled-transaction
  [{:scheduled-transaction/keys [items] :keys [id] :as trx}]
  (cons (dissoc trx :scheduled-transaction/items)
        (map #(assoc % :scheduled-transaction-item/scheduled-transaction-id id)
             items)))

(defn- ->keyword
  [x]
  (if (string? x)
    (keyword x)
    x))

(defmethod sql/before-save :scheduled-transaction
  [trx]
  (update-in trx [:scheduled-transaction/period 1] name))

(defmethod sql/after-read :scheduled-transaction
  [trx]
  (-> trx
      (update-in [:scheduled-transaction/period 0] parse-int)
      (update-in [:scheduled-transaction/period 1] keyword)
      (update-in-if [:scheduled-transaction/start-date] t/local-date)
      (update-in-if [:scheduled-transaction/end-date] t/local-date)
      (update-in-if [:scheduled-transaction/date-spec :day] ->keyword)
      (update-in-if [:scheduled-transaction/date-spec :days] (comp set
                                                                   #(map ->keyword %)))))

(defmethod sql/post-select :scheduled-transaction
  [{:keys [storage]} trxs]
  (map #(assoc %
               :scheduled-transaction/items
               (vec (db/select storage
                               {:scheduled-transaction-item/scheduled-transaction (util/->model-ref %)}
                               {})))
       trxs))
