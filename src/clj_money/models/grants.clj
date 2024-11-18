(ns clj-money.models.grants
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.walk :refer [keywordize-keys]]
            [config.core :refer [env]]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.validation :refer [with-validation]]
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

(defn ^:deprecated search
  ([criteria]
   (search criteria {}))
  ([_criteria _options]
   (throw (UnsupportedOperationException. "search is deprecated"))))

(defn ^:deprecated find-by
  ([criteria] (find-by criteria {}))
  ([_criteria _options]
   (throw (UnsupportedOperationException. "find-by is deprecated"))))

(defn has-permission?
  [grant resource-type action]
  (when grant ; TODO: change this not to tolerate nil values
    (action
     (->> grant
          :permissions
          resource-type))))
