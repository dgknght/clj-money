(ns clj-money.authorization
  (:require [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [cemerick.friend :refer [current-authentication]])
  (:import clj_money.NotAuthorizedException))

(defn- resource-key
  "Returns a keyword identifying the type of the resource"
  [resource]
  (if (keyword? resource)
    resource
    (-> resource meta :resource-type)))

(defmulti allowed?
  "Returns a truthy or falsey value indicating whether or not the
  authenticated user is allowed to perform the specified
  action on the specified resource"
  (fn [user action resource params]
    [(resource-key resource) action]))

(defn authorize
  [action resource params]
  (if-not (allowed? (current-authentication) action resource params)
    (throw (NotAuthorizedException.))))

(defn user-owns-entity?
  [user entity-id]
  (contains? (->> (clj-money.models.entities/select (env :db) (:id user))
                  (map :id)
                  (into #{}))
             entity-id))

; Accounts
; --------

(defmethod allowed? [:account :index]
  [user action resource params]
  (= (:id user)
     (-> params :entity :user-id)))

(defmethod allowed? [:account :new]
  [user action resource params]
  (= (:id user)
     (-> params :entity :user-id)))

(defmethod allowed? [:account :create]
  [user action resource params]
  (= (:id user)
     (-> params :entity :user-id)))

(defmethod allowed? [:account :show]
  [user action resource params]
  (user-owns-entity? user (:entity-id resource)))

(defmethod allowed? [:account :edit]
  [user action resource params]
  (user-owns-entity? user (:entity-id resource)))

(defmethod allowed? [:account :update]
  [user action resource params]
  (user-owns-entity? user (:entity-id resource)))

(defmethod allowed? [:account :delete]
  [user action resource params]
  (user-owns-entity? user (:entity-id resource)))

; Transactions
; ------------

(defmethod allowed? [:transaction :index]
  [user action resource params]
  (= (:id user)
     (-> params :entity :user-id)))

(defmethod allowed? [:transaction :new]
  [user action resource params]
  (= (:id user)
     (-> params :entity :user-id)))

(defmethod allowed? [:transaction :create]
  [user action resource params]
  (= (:id user)
     (-> params :entity :user-id)))

(defmethod allowed? [:transaction :edit]
  [user action resource params]
  (user-owns-entity? user (:entity-id resource)))

(defmethod allowed? [:transaction :update]
  [user action resource params]
  (user-owns-entity? user (:entity-id resource)))

(defmethod allowed? [:transaction :delete]
  [user action resource params]
  (user-owns-entity? user (:entity-id resource)))
