(ns clj-money.db.sql.transaction-items
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.db.sql :as sql]
            [clj-money.util :refer [temp-id?]]))

(defmethod sql/resolve-temp-ids :transaction-item
  [{:transaction-item/keys [transaction-id]
    :as item}
   id-map]
  (cond-> item
    (temp-id? transaction-id)
    (update-in [:transaction-item/transaction-id] id-map)))
