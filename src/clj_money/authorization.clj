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
                 (-> resource meta :resource-type))]
    (if result
      result
      (throw (ex-info "Unable to determine the resource type." {:resource resource})))))

(def ^:private auth-fns (atom {}))

(defn allowed?
  "Returns a truthy or falsey value indicating whether or not the
  authenticated user is allowed to perform the specified
  action on the specified resource"
  [& args]
  (let [[user
         action
         resource
         params] (case (count args)
                   4 args
                   3 (concat [(current-authentication)] args)
                   :else (throw (ex-info "Wrong number of arguments. Expected 3 or 4." {:args args})))
        rkey (resource-key resource)
        auth-fn (@auth-fns [action rkey])]
    (if auth-fn
      (auth-fn
        user
        resource
        params)
      (throw (ex-info "No authorization rule registered"
                      {:action action :resource resource-key})))))

(defn authorize
  [action resource params]
  (if-not (allowed? (current-authentication) action resource params)
    (throw (NotAuthorizedException.))))

(defn allow
  [resource actions auth-fn]
  (swap! auth-fns #(reduce (fn [result action]
                             (assoc result [action resource] auth-fn))
                           %
                           actions)))

(defn user-owns-entity?
  [user entity-id]
  (contains? (->> (clj-money.models.entities/select (env :db) (:id user))
                  (map :id)
                  (into #{}))
             entity-id))

; Entities
; --------

(allow :entity [:show :edit :update :delete]
       (fn [user resource params]
         (= (:id user) (:user-id resource))))

; Accounts
; --------

(allow :account [:create :show :edit :update :delete]
       (fn [user resource params]
         (user-owns-entity? user (:entity-id resource))))

; Transactions
; ------------

;(defmethod allowed? [:transaction :index]
;  [user action resource params]
;  (= (:id user)
;     (-> params :entity :user-id)))
;
;(defmethod allowed? [:transaction :new]
;  [user action resource params]
;  (= (:id user)
;     (-> params :entity :user-id)))
;
;(defmethod allowed? [:transaction :create]
;  [user action resource params]
;  (= (:id user)
;     (-> params :entity :user-id)))
;
;(defmethod allowed? [:transaction :edit]
;  [user action resource params]
;  (user-owns-entity? user (:entity-id resource)))
;
;(defmethod allowed? [:transaction :update]
;  [user action resource params]
;  (user-owns-entity? user (:entity-id resource)))
;
;(defmethod allowed? [:transaction :delete]
;  [user action resource params]
;  (user-owns-entity? user (:entity-id resource)))
