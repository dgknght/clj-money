(ns clj-money.db.sql.queries
  (:refer-clojure :exclude [format])
  (:require [clojure.pprint :refer [pprint]]
            [stowaway.sql-qualified :as sql]))

(def ^:private default-options
  {:relationships #{[:users :identities]
                    [:users :entities]
                    [:entities :commodities]
                    [:entities :accounts]
                    [:entities :transactions]
                    [:transactions :transaction_items]}})

(defn criteria->query
  [criteria & [options]]
  {:pre [criteria]}

  (sql/->query criteria (merge default-options options)))
