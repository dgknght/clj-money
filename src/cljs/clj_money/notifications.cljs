(ns clj-money.notifications
  (:require [reagent.core :as r]))

(defonce notifications (r/atom []))

(defn notify
  ([message] (notify message :info))
  ([message severity]
   (swap! notifications #(conj % {:message message
                                  :severity severity}))))

(defn- remove-at
  [coll index]
  (vec (concat (subvec coll 0 index)
               (subvec coll (inc index)))))

(defn unnotify
  [index]
  (swap! notifications #(remove-at % index)))
