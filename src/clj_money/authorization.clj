(ns clj-money.authorization
  (:require [clojure.tools.logging :as log]
            [cemerick.friend :refer [current-authentication]])
  (:import clj_money.NotAuthorizedException))

(defn- find-policy
  [resource]
  {})

(defn- allowed?
  [policy action resource params]

  (log/debug "allowed?" (prn-str {:user-id (:id (current-authentication))
                                  :entity-user-id (-> params :entity :user-id)}))

  (= (:id (current-authentication))
     (-> params :entity :user-id)))

(defn authorize
  [action resource params]
  (when-let [policy (find-policy resource)]
    (if-not (allowed? policy action resource params)
      (throw (NotAuthorizedException.)))))
