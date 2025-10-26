(ns clj-money.authorization
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.util :as util]))

(derive ::create ::manage)
(derive ::show ::manage)
(derive ::update ::manage)
(derive ::destroy ::manage)

(defmulti allowed?
  "Returns a truthy or falsey value indicating whether or not the
  authenticated user is allowed to perform the specified
  action on the specified entity"
  (fn [entity action _user]
    (let [entity-type (util/entity-type entity)]
      (assert entity-type (prn-str entity))
      [entity-type action])))

(defn opaque?
  [error]
  (::opaque? (ex-data error)))

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
  has access."
  (fn
    [entity-type _user]
    entity-type))

(defn +scope
  ([criteria user]
   (+scope criteria (util/entity-type criteria) user))
  ([criteria entity-type user]
   {:pre [entity-type user]}

   (if-let [s (scope entity-type user)]
     (if (empty? criteria)
       s
       (vary-meta [:and criteria s]
                  #(merge {:clj-money/entity-type entity-type}
                          %)))
     criteria)))
