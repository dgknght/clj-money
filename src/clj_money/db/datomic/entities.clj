(ns clj-money.db.datomic.entities
  (:require [clojure.set :refer [difference]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-yaml.core :as yaml]
            [clj-money.entities :as ents]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/before-save :entity
  [entity]
  (-> entity
      (update-in-if [:entity/settings :settings/expense-hints] yaml/generate-string)
      (update-in-if [:entity/settings :settings/budget-tags] pr-str)
      (update-in-if [:entity/settings :settings/monitor-order] pr-str)))

(defmethod datomic/after-read :entity
  [entity]
  (update-in-if entity [:entity/settings :settings/expense-hints] yaml/parse-string))

(def ^:private account-set-attrs
  "Settings that hold a set of accounts. A ref with cardinality many only
  adds values when saved, so removed accounts have to be retracted."
  [:settings/monitored-accounts
   :settings/payment-methods
   :settings/expense-accounts])

(defn- retractions
  [entity settings-id attr]
  (->> (difference
         (set (-> entity ents/before :entity/settings attr))
         (set (-> entity :entity/settings attr)))
       (map (fn [account-ref]
              [:db/retract settings-id attr (:id account-ref)]))))

(defmethod datomic/deconstruct :entity
  [entity]
  (let [settings-id (-> entity ents/before :entity/settings :id)]
    (cons entity
          (when settings-id
            (mapcat (partial retractions entity settings-id)
                    account-set-attrs)))))
