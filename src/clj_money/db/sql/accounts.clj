(ns clj-money.db.sql.accounts
  (:require [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :as types]))

(defn- parse-allocations
  [allocations]
  (some-> allocations
          (update-vals bigdec)
          (update-keys (types/qid :account))))

(defmethod sql/after-read :account
  [account]
  (-> account
      (update-in [:account/allocations] parse-allocations)
      (update-in-if [:account/transaction-date-range 0] t/local-date)
      (update-in-if [:account/transaction-date-range 1] t/local-date)))
