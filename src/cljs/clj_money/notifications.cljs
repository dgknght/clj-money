(ns clj-money.notifications
  (:require [reagent.core :as r]
            [goog.string :as gstr]))

(defonce notifications (r/atom []))

(defn notify
  ([message] (notify message :info))
  ([message severity]
   (let [notification {:message message
                       :severity severity
                       :id (random-uuid)}]
     (swap! notifications #(conj % notification)))))

(defn success [message] (notify message :success))
(defn info    [message] (notify message :info))
(defn warning [message] (notify message :warning))
(defn warn    [message] (notify message :warning))
(defn danger  [message] (notify message :danger))

(defn danger-fn
  [msg-format]
  (fn [msg]
    (danger (gstr/format msg-format msg))))

(defn- remove-at
  [coll index]
  (vec (concat (subvec coll 0 index)
               (subvec coll (inc index)))))

(defn unnotify
  [notification]
  (swap! notifications (fn [notifications]
                         (remove #(= (:id %) (:id notification))
                                 notifications))))
