(ns clj-money.notifications
  (:require [reagent.core :as r]
            [goog.string :as gstr]
            [cljs-time.core :as t]))

(defonce notifications (r/atom []))
(defonce toasts (r/atom []))

(def ^:const toast-delay 2000)
(def ^:const toast-sweep-interval 2500)

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
    (danger (gstr/format msg-format (or (:message msg) msg)))))

(defn unnotify
  [notification]
  (swap! notifications (fn [notifications]
                         (remove #(= (:id %) (:id notification))
                                 notifications))))

(declare sweep-toasts)
(defn- queue-toast-sweep []
  (.setTimeout js/window sweep-toasts toast-sweep-interval))

(defn toast
  [title body]
  (swap! toasts conj {:title title
                      :body body
                      :expires-at (t/from-now (t/seconds 2))
                      :id (str (random-uuid))})
  (queue-toast-sweep))

(defn toastf
  [title body-pattern & args]
  (toast title (apply gstr/format body-pattern args)))

(defn untoast
  [id]
  (swap! toasts (fn [toasts]
                  (remove #(= id (:id %)) toasts))))

(defn- expired-toast?
  [{:keys [expires-at]}]
  (t/after? (t/now) expires-at))

(defn- sweep-toasts
  ([]
   (swap! toasts sweep-toasts))
  ([toasts]
   (let [result (remove expired-toast? toasts)]
     (when (seq result)
       (queue-toast-sweep))
     result)))
