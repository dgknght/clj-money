(ns clj-money.authorization
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [cemerick.friend :refer [current-authentication]]
            [clj-money.util :refer [pprint-and-return]])
  (:import clj_money.NotAuthorizedException))

(defn- resource-key
  "Returns a keyword identifying the type of the resource"
  [resource]
  (let [result (if (keyword? resource)
                 resource
                 (-> resource meta ::resource-type))]
    (if result
      result
      (throw (ex-info "Unable to determine the resource type." {:resource resource})))))

(defn tag-resource
  "Adds meta data to identity the type of the specified resource"
  [resource type-key]
  (vary-meta resource #(assoc % ::resource-type type-key)))

(def ^:private auth-context (atom {}))

(defn ->context
  [key value]
  (swap! auth-context #(assoc % key value)))

(def ^:private auth-fns (atom {}))

(defn allowed?
  "Returns a truthy or falsey value indicating whether or not the
  authenticated user is allowed to perform the specified
  action on the specified resource"
  [& args]
  (let [[user
         action
         resource] (case (count args)
                   3 args
                   2 (concat [(current-authentication)] args)
                   :else (throw (ex-info "Wrong number of arguments. Expected 2 or 3." {:args args})))
        rkey (resource-key resource)
        auth-fn (@auth-fns [action rkey])]
    (if auth-fn
      (auth-fn
        user
        resource
        @auth-context)
      (throw (ex-info (format "No authorization rule registered for %s %s." action rkey)
                      {:action action :resource resource-key})))))

(defn authorize
  "Raises an error if the current user does not have
  permission to perform the specified function.

  This function returns the resource so that it can be threaded together
  with other left-threadable operations"
  [resource action]
  (if (allowed? (current-authentication) action resource)
    resource
    (throw (NotAuthorizedException.))))

(defn allow
  "Registers a rule that will be used to determine if the
  current user has permission to perform a requested function

  The function needs to accept three arguments:
    user: The current user, whose permissions are being queried
    action: A keyword indicating the action for which permission is being queried
    resource: The resource on which permission is being queried"
  [resource actions auth-fn]
  (swap! auth-fns #(reduce (fn [result action]
                             (assoc result [action resource] auth-fn))
                           %
                           actions)))

(def ^:private scope-maps
  (atom {}))

(defn set-scope
  "Registers a scope function map with a resource type.

  A scope function map is a map of attribute names to 
  functions that will be used at run time to get the
  broadest available criteria for the authenticated 
  resource to use to query that type of resource"
  [resource-type fn-map]
  (swap! scope-maps #(assoc % resource-type fn-map)))

(defn apply-scope
  "Applies the registered scope function map to the specified
  criteria in order to ensure the query does not extend
  beyond the scope the user is authorized to access."
  [criteria resource-type]
  (let [user (current-authentication)]
    (->> (resource-type @scope-maps)
         (map (fn [[attribute scope-fn]]
                [attribute (scope-fn user @auth-context)]))
         (into {})
         (merge-with (fn [requested allowed]
                       (or (allowed requested)
                           (throw (NotAuthorizedException. "query out of scope"))))
                     criteria))))
