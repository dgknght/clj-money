(ns clj-money.api.lot-notes
  (:require [clj-money.api :as api :refer [add-error-handler]]))

(defn select
  [{:lot-note/keys [lot] :as criteria} & {:as opts}]
  (api/get (api/path :lots lot :lot-notes)
           (dissoc criteria :lot-note/lot)
           (add-error-handler opts "Unable to retrieve lot notes: %s")))

(defn create
  [{:lot-note/keys [lot] :as note} & {:as opts}]
  (api/post (api/path :lots lot :lot-notes)
            (dissoc note :lot-note/lot)
            (add-error-handler opts "Unable to create lot note: %s")))

(defn delete
  [note & {:as opts}]
  (api/delete (api/path :lot-notes (:id note))
              (add-error-handler opts "Unable to delete lot note: %s")))
