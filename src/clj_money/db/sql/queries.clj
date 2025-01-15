(ns clj-money.db.sql.queries
  (:refer-clojure :exclude [format])
  (:require [clojure.pprint :refer [pprint]]
            [stowaway.sql-qualified :as sql]
            [clj-money.db :as db]))

(def ^:private default-options
  {:relationships #{[:users :identities]
                    [:users :entities]
                    [:entities :commodities]
                    [:entities :accounts]
                    [:entities :transactions]
                    [:entities :budgets]
                    [:accounts :transaction_items]
                    [:accounts :reconciliations]
                    [:transactions :transaction_items]
                    [:transactions :attachments]
                    [:transactions :lot_items]
                    [:images :attachments]
                    [:entities :scheduled_transactions]
                    [:commodities :lots]
                    [:commodities :prices]
                    [:accounts :lots]}})

(defn criteria->query
  [criteria & [options]]
  {:pre [criteria
         (db/model-type criteria)]}
  (sql/->query criteria (merge default-options options)))

(defn ->update
  [changes criteria & [options]]
  (sql/->update changes criteria (merge default-options options)))
