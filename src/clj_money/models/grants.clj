(ns clj-money.models.grants
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clj-money.models :as models]))

(def resource-types
  #{:account
    :entity
    :transaction
    :budget
    :budget-item
    :commodity
    :price
    :attachment
    :reconciliation
    :report})

(def actions
  #{:show :create :update :delete :index})

(s/def :grant/entity ::models/model-ref)
(s/def :grant/user ::models/model-ref)
(s/def :grant/permissions (s/map-of resource-types
                                    (s/coll-of actions
                                               :min-count 1
                                               :kind set?)))
(s/def ::models/grant (s/keys :req [:grant/entity
                                    :grant/user
                                    :grant/permissions]))

(defn has-permission?
  [grant resource-type action]
  (when grant ; TODO: change this not to tolerate nil values
    (action
     (->> grant
          :grant/permissions
          resource-type))))
