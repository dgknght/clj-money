(ns clj-money.db.datomic.entities
  (:require [clojure.set :refer [difference]]
            [clj-money.entities :as ents]
            [clj-money.db.datomic :as datomic]))

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
