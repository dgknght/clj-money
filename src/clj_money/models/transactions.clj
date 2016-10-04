(ns clj-money.models.transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [schema.core :as schema]))

(defn create
  "Creates a new transaction"
  [storage-spec transaction]
  {})
