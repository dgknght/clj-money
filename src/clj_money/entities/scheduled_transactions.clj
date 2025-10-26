(ns clj-money.entities.scheduled-transactions
  (:refer-clojure :exclude [find update])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.validation :as v]
            [clj-money.entities :as entities]
            [clj-money.dates :as dates]
            [clj-money.entities.transactions :as trans]))

(defn- debit-credit-balanced?
  [items]
  (let [totals (->> items
                    (group-by :scheduled-transaction-item/action)
                    (map (fn [entry]
                           (update-in entry
                                      [1]
                                      #(->> %
                                            (map :scheduled-transaction-item/quantity)
                                            (reduce + 0M)))))
                    (into {}))]
    (= (:debit totals)
       (:credit totals))))

(v/reg-msg debit-credit-balanced? "The sum of debits must equal the sum of credits")

(defn- greater-than-zero?
  [n]
  (< 0 n))
(v/reg-msg greater-than-zero? "%s must be greater than zero")

(s/def :scheduled-transaction-item/action trans/actions)
(s/def :scheduled-transaction-item/account ::entities/entity-ref)
(s/def :scheduled-transaction-item/quantity v/positive-big-dec?)
(s/def ::entities/scheduled-transaction-item (s/keys :req [:scheduled-transaction-item/action
                                                         :scheduled-transaction-item/account
                                                         :scheduled-transaction-item/quantity]))

(s/def :scheduled-transaction/items (s/and (s/coll-of ::entities/scheduled-transaction-item
                                                      :min-count 2)
                                           debit-credit-balanced?))
(s/def :scheduled-transaction/entity ::entities/entity-ref)
(s/def :scheduled-transaction/description string?)
(s/def :scheduled-transaction/period ::dates/period)
(s/def :scheduled-transaction/start-date t/local-date?)
(s/def :scheduled-transaction/end-date (s/nilable t/local-date?))
(s/def ::month (s/and integer?
                      #(<= 1 % 12)))
(s/def ::day (s/or :number (s/and integer?
                                  #(<= 1 % 31))
                   :last #{:last}))
(s/def ::days (s/coll-of #{:sunday :monday :tuesday :wednesday :thursday :friday :saturday}
                         :kind set?))
(s/def ::date-spec (s/or :yearly (s/keys :req-un [::month ::day])
                         :monthly (s/keys :req-un [::day])
                         :weekly (s/keys :req-un [::days])))
(s/def :scheduled-transaction/date-spec ::date-spec)
^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(s/def ::entities/scheduled-transaction (s/keys :req [:scheduled-transaction/entity
                                                    :scheduled-transaction/description
                                                    :scheduled-transaction/period
                                                    :scheduled-transaction/start-date
                                                    :scheduled-transaction/date-spec
                                                    :scheduled-transaction/items]
                                              :opt [:scheduled-transaction/end-date]))
