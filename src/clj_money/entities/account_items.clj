(ns clj-money.entities.account-items
  (:require [clojure.spec.alpha :as s]
            [clj-money.entities :as e]))

(s/def :account-item/reconciliation ::e/entity-ref)
(s/def :account-item/index integer?)
(s/def :account-item/balance decimal?)
(s/def :account-item/quantity decimal?)
(s/def ::e/account-item (s/keys :req [:account-item/quantity
                                      :account-item/balance
                                      :account-item/index]
                                :opt [:account-item/reconciliation]))
