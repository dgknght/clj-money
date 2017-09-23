(ns clj-money.shared
  (:refer-clojure :exclude [update])
  (:require [environ.core :refer [env]]
            [clj-money.models.entities :as entities]))

; TODO move this to auth-helpers
(defn user-owns-entity?
  [user entity-id]
  (contains? (->> (entities/select (env :db) (:id user))
                  (map :id)
                  (into #{}))
             entity-id))
