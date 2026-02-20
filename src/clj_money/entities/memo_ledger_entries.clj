(ns clj-money.entities.memo-ledger-entries
  (:require [clojure.spec.alpha :as s]
            [java-time.api :as t]
            [clj-money.entities :as entities]))

(s/def :memo-ledger-entry/lot ::entities/entity-ref)
(s/def :memo-ledger-entry/transaction-date t/local-date?)
(s/def :memo-ledger-entry/memo string?)
^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(s/def ::entities/memo-ledger-entry
  (s/keys :req [:memo-ledger-entry/lot
                :memo-ledger-entry/transaction-date
                :memo-ledger-entry/memo]))
