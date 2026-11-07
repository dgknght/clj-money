(ns clj-money.pending-review
  "Keeps the count of transactions awaiting review (e.g., read from a
  receipt or a statement), shown on the Review nav item"
  (:require [clj-money.state :as state]
            [clj-money.api.transactions :as trxs]))

(defn load-count
  "Loads the number of transactions in the current entity awaiting review"
  []
  (trxs/count #:transaction{:review-status :pending}
              :on-success #(reset! state/pending-review-count (:count %))))
