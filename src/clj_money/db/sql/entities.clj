(ns clj-money.db.sql.entities
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.util :as util]
            [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :as types]))

(defmethod sql/after-read :entity
  [entity]
  (-> entity
      (update-in-if [:entity/settings :settings/default-commodity :id]
                    (types/qid :commodity))
      (update-in-if [:entity/settings :settings/lt-capital-gains-account :id]
                    (types/qid :account))
      (update-in-if [:entity/settings :settings/lt-capital-loss-account :id]
                    (types/qid :account))
      (update-in-if [:entity/settings :settings/st-capital-gains-account :id]
                    (types/qid :account))
      (update-in-if [:entity/settings :settings/st-capital-loss-account :id]
                    (types/qid :account))
      (update-in-if [:entity/settings :settings/monitored-accounts]
                    (fn [refs]
                      (->> refs
                           (map #(update-in % [:id] (types/qid :account)))
                           set)))
      (update-in-if [:entity/settings :settings/inventory-method] keyword)
      (update-in-if [:entity/transaction-date-range 0] t/local-date)
      (update-in-if [:entity/transaction-date-range 1] t/local-date)
      (update-in-if [:entity/price-date-range 0] t/local-date)
      (update-in-if [:entity/price-date-range 1] t/local-date)))

(defn- resolve-temp-id
  [id-map]
  (fn [id]
    (if (util/temp-id? id)
      (id-map id)
      id)))

(defmethod sql/resolve-temp-ids :entity
  [entity id-map]
  (-> entity
      (update-in-if [:entity/settings :settings/lt-capital-gains-account :id] (resolve-temp-id id-map))
      (update-in-if [:entity/settings :settings/lt-capital-loss-account :id] (resolve-temp-id id-map))
      (update-in-if [:entity/settings :settings/st-capital-gains-account :id] (resolve-temp-id id-map))
      (update-in-if [:entity/settings :settings/st-capital-loss-account :id] (resolve-temp-id id-map))))
