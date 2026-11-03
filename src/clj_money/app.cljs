(ns clj-money.app
  (:require [accountant.core :as accountant]
            [clj-money.util :as util]
            [clj-money.state :as state :refer [+busy -busy]]
            [clj-money.api.entities :as entities]
            [clj-money.api.scheduled-transactions :as sched-trxs]
            [clj-money.pending-review :as pending-review]))

(defn load-pending-count []
  (try
    (sched-trxs/count
      {:pending true}
      :on-success #(reset! state/pending-scheduled-count (:count %)))
    (catch js/Error e
      (.error js/console
              "Unable to fetch the count of pending scheduled transactions."
              e))))

(defn- load-pending-counts []
  (load-pending-count)
  (pending-review/load-count))

(defn watch-entity
  [_ _ previous current]
  (when-not (util/id= previous current)
    (if current
      (load-pending-counts)
      (do (reset! state/pending-scheduled-count 0)
          (reset! state/pending-review-count 0)))))

(defn- receive-entities
  [on-complete]
  (fn [[entity :as entities]]
    (let [previous @state/current-entity]
      (state/set-entities entities)
      ; on page load, the entity restored from the cookie has the same id,
      ; so watch-entity doesn't load the counts
      (when (and @state/current-entity
                 (util/id= previous @state/current-entity))
        (load-pending-counts)))
    (if entity
      (when on-complete (on-complete))
      (accountant/navigate! "/entities"))))

(defn fetch-entities
  [& {:keys [on-complete]}]
  (+busy)
  (entities/select :callback -busy
                   :on-success (receive-entities on-complete)))
