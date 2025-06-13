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
  action on the specified model"
  (fn [model action _user]
    (let [model-type (util/model-type model)]
      (assert model-type (prn-str model))
      [model-type action])))

(defn opaque?
  [error]
  (::opaque? (ex-data error)))

(defn- auth-error
  [model action opaque?]
  (let [[msg err-type] (if opaque?
                         ["not found" ::not-found]
                         ["forbidden" ::forbidden])]
    (ex-info msg {:type err-type
                  :action action
                  :model (util/model-type model)
                  ::opaque? opaque?})))

(defn authorize
  "Raises an error if the current user does not have
  permission to perform the specified function.

  This function returns the model so that it can be threaded together
  with other left-threadable operations"
  [model action user]
  {:pre [action user]}
  (when model
    (if (allowed? model action user)
      model
      (throw (auth-error model action (not (allowed? model ::show user)))))))

(defmulti scope
  "Returns a criteria structure limiting the scope
  of a query to that to which the specified user
  has access."
  (fn
    [model-type _user]
    model-type))

(defn +scope
  ([criteria user]
   (+scope criteria (util/model-type criteria) user))
  ([criteria model-type user]
   {:pre [model-type user]}

   (if-let [s (scope model-type user)]
     (if (empty? criteria)
       s
       (vary-meta [:and criteria s]
                  #(merge {:clj-money/model-type model-type}
                          %)))
     criteria)))
