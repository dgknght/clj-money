(ns clj-money.models.transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [schema.core :as schema]
            [clj-money.models.helpers :refer [storage]]
            [clj-money.models.storage :refer [create-transaction]])
  (:import java.util.Date
           org.joda.time.DateTime))

(def NewTransaction
  {:entity-id schema/Int
   :transaction-date DateTime
   :items [{:account-id schema/Int
            :action (schema/enum :debit :credit)
            :amount BigDecimal}]})

(defn create
  "Creates a new transaction"
  [storage-spec transaction]
  (create-transaction (storage storage-spec) (dissoc transaction :items)))
