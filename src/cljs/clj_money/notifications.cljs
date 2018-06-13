(ns clj-money.notifications
  (:require [reagent.core :as r]))

(defonce notifications (r/atom []))

(defn notify
  ([message] (notify message :info))
  ([message severity]
   (swap! notifications #(conj % {:message message
                                  :severity severity}))))

(defn success [message] (notify message :success))
(defn info    [message] (notify message :info))
(defn warning [message] (notify message :warning))
(defn warn    [message] (notify message :warning))
(defn danger  [message] (notify message :danger))

(defn- remove-at
  [coll index]
  (vec (concat (subvec coll 0 index)
               (subvec coll (inc index)))))

(defn unnotify
  [notification]
  (swap! notifications (fn [notifications]
                         (remove notifications #(= % notification)))))
