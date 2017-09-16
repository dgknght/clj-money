(ns clj-money.authorization
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [cemerick.friend :refer [current-authentication]])
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
        resource)
      (throw (ex-info (format "No authorization rule registered for %s %s." action rkey)
                      {:action action :resource resource-key})))))

(defn authorize
  [action resource]
  (if-not (allowed? (current-authentication) action resource)
    (throw (NotAuthorizedException.))))

(defn allow
  [resource actions auth-fn]
  (swap! auth-fns #(reduce (fn [result action]
                             (assoc result [action resource] auth-fn))
                           %
                           actions)))
