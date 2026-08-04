(ns clj-money.cached-accounts
  (:require [cljs.pprint :refer [pprint]]
            [clj-money.util :as util]
            [clj-money.dates :refer [push-entity-boundary]]
            [clj-money.state :as state]
            [clj-money.accounts :refer [nest unnest]]
            [clj-money.api.accounts :as accts]))

(defn fetch-accounts
  [& {:keys [post-xf]}]
  (accts/select
    {}
    :on-success #(reset! state/accounts (->> % nest unnest (into [])))
    :post-xf (or post-xf
                 (map identity))))

(defn latest
  [{:keys [id]}]
  (@state/accounts-by-id id))

(defn push-transaction-date!
  "Given an account and the date of a transaction that touches it, extends
  the account's cached :account/transaction-date-range to include that
  date, upserts the result into the shared accounts cache, and returns the
  updated account."
  [account date]
  (let [updated (push-entity-boundary account :account/transaction-date-range date)]
    (swap! state/accounts #(util/upsert-into updated {:sort-key :account/path} %))
    updated))

(defn watch-entity
  [_ _ previous current]
  (when-not (util/id= previous current)
    (reset! state/accounts nil))
  (when current
    (fetch-accounts)))
