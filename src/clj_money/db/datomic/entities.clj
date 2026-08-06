(ns clj-money.db.datomic.entities
  (:require [clojure.set :refer [difference]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.entities :as ents]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/before-save :entity
  [entity]
  (update-in-if entity
                [:entity/settings :settings/budget-tags]
                pr-str))

(defmethod datomic/deconstruct :entity
  [entity]
  (let [settings-id (-> entity ents/before :entity/settings :id)
        removed (when settings-id
                  (difference
                    (set (-> entity ents/before :entity/settings :settings/monitored-accounts))
                    (set (-> entity :entity/settings :settings/monitored-accounts))))]
    (cons entity
          (map (fn [account-ref]
                 [:db/retract settings-id :settings/monitored-accounts (:id account-ref)])
               removed))))
