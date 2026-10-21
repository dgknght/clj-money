(ns clj-money.authorization
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.util :as util]))

(derive ::index ::manage)
(derive ::create ::manage)
(derive ::show ::manage)
(derive ::update ::manage)
(derive ::destroy ::manage)
(derive ::recalculate ::manage)

(defmulti allowed?
  "Returns a truthy or falsey value indicating whether or not the
  authenticated user is allowed to perform the specified
  action on the specified entity"
  (fn [entity action _user]
    (let [entity-type (util/entity-type entity)]
      (assert entity-type (prn-str entity))
      [entity-type action])))

(defn allow?
  ([action user]
   #(allow? % action user))
  ([entity action user]
   (allowed? entity action user)))

(defn- auth-error
  [entity action opaque?]
  (let [[msg err-type] (if opaque?
                         ["not found" ::not-found]
                         ["forbidden" ::forbidden])]
    (ex-info msg {:type err-type
                  :action action
                  :entity (util/entity-type entity)
                  ::opaque? opaque?})))

(defn authorize
  "Raises an error if the current user does not have
  permission to perform the specified function.

  This function returns the entity so that it can be threaded together
  with other left-threadable operations"
  [entity action user]
  {:pre [action user]}
  (when entity
    (if (allowed? entity action user)
      entity
      (throw (auth-error entity action (not (allowed? entity ::show user)))))))

(defmulti scope
  "Returns a criteria structure limiting the scope
  of a query to that to which the specified user
  has access for the action: ::index to list resources,
  or ::manage to look one up for authorize to check
  the action taken on it."
  (fn
    [entity-type _user _action]
    entity-type))

(defn +scope
  "Limits the criteria to what the user can list, or, given
  ::manage as the action, to what the user may hold any
  permission on, for a lookup that authorize checks next"
  ([criteria user]
   (+scope criteria (util/entity-type criteria) user))
  ([criteria entity-type user]
   (+scope criteria entity-type user ::index))
  ([criteria entity-type user action]
   {:pre [entity-type user action]}

   (if-let [s (scope entity-type user action)]
     (if (empty? criteria)
       (cond-> s (empty? s) (util/entity-type entity-type))
       (vary-meta [:and criteria s]
                  #(merge {:clj-money/entity-type entity-type}
                          %)))
     criteria)))
