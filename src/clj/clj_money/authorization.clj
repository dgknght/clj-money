(ns clj-money.authorization
  (:require [clj-money.models :as models]
            [slingshot.slingshot :refer [throw+]]))

(derive ::create ::manage)
(derive ::show ::manage)
(derive ::update ::manage)
(derive ::destroy ::manage)

(defmulti allowed?
  "Returns a truthy or falsey value indicating whether or not the
  authenticated user is allowed to perform the specified
  action on the specified model"
  (fn [model action _user]
    [(models/tag model) action]))

(defn authorize
  "Raises an error if the current user does not have
  permission to perform the specified function.

  This function returns the model so that it can be threaded together
  with other left-threadable operations"
  [model action user]
  {:pre [model action user]}
  (if (allowed? model action user)
      model
      (throw+ {:type ::unauthorized
               :action action
               :model (models/tag model)})))

(defmulti scope
  "Returns a criteria structure limiting the scope
  of a query to that to which the specified user
  has access."
  (fn
    [model-type _user]
    model-type))

(defn +scope
  ([criteria user]
   (+scope criteria (models/tag criteria) user))
  ([criteria model-type user]
   (if-let [s (scope model-type user)]
     (if (empty? criteria)
       s
       [:and criteria s])
     criteria)))
