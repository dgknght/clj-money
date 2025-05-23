(ns clj-money.db.sql.queries
  (:refer-clojure :exclude [format])
  (:require [clojure.pprint :refer [pprint]]
            [stowaway.sql-qualified :as sql]
            [clj-money.util :as util]))

(def ^:private default-options
  {:relationships #{[:user :identity]
                    [:user :entity]
                    [:entity :commodity]
                    [:entity :account]
                    [:entity :transaction]
                    [:entity :budget]
                    [:account :transaction_item]
                    [:account :reconciliation]
                    [:transaction :transaction_item]
                    [:transaction :attachment]
                    [:transaction :lot_item]
                    [:image :attachment]
                    [:entity :scheduled_transaction]
                    [:commodity :lot]
                    [:commodity :price]
                    [:account :lot]}})

(defn criteria->query
  [criteria & [options]]
  {:pre [criteria
         (util/model-type criteria)]}
  (sql/->query criteria (merge default-options options)))

(defn ->update
  [changes criteria & [options]]
  (sql/->update changes criteria (merge default-options options)))
