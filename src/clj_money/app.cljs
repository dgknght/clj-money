(ns clj-money.app
  (:require [accountant.core :as accountant]
            [clj-money.state :as state :refer [+busy -busy]]
            [clj-money.api.entities :as entities]
            [clj-money.api.scheduled-transactions :as sched-trxs]))

(defn- load-pending-count []
  (try
    (sched-trxs/count
      {:pending true}
      :on-success #(reset! state/pending-scheduled-count (:count %)))
    (catch js/Error e
      (.error js/console
              "Unable to fetch the count of pending scheduled transactions."
              e))))

(defn- receive-entities
  [[entity :as entities] on-complete]
  (state/set-entities entities)
  (if entity
    (do
      (load-pending-count)
      (when on-complete (on-complete)))
    (accountant/navigate! "/entities")))

(defn fetch-entities
  ([] (fetch-entities nil))
  ([on-complete]
   (+busy)
   (entities/select :callback -busy
                    :on-success #(receive-entities % on-complete))))
