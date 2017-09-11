(ns clj-money.authorization
  (:require [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [cemerick.friend :refer [current-authentication]])
  (:import clj_money.NotAuthorizedException))

(defn- allowed?
  [user action resource params]
  (case action
    :index
    (= (:id user)
       (-> params :entity :user-id))

    :show
    (let [entity-ids (->> (clj-money.models.entities/select (env :db) (:id user))
                          (map :id)
                          (into #{}))]
      (entity-ids (:entity-id resource)))))

(defn authorize
  [action resource params]
  (if-not (allowed? (current-authentication) action resource params)
    (throw (NotAuthorizedException.))))
