(ns clj-money.api.lot-notes
  (:require [clj-money.api :as api :refer [add-error-handler]]))

(defn select
  [{:as criteria :lot/keys [account]} & {:as opts}]
  {:pre [(:lot/account criteria)]}
  (api/get (api/path :accounts
                     account
                     :lot-notes)
           (dissoc criteria :lot/account)
           (add-error-handler opts "Unable to retrieve lot notes: %s")))

(defn create
  [note & {:as opts}]
  {:pre [(:commodity opts)]}
  (api/post (api/path :commodities
                      (:commodity opts)
                      :lot-notes)
            note
            (add-error-handler opts "Unable to create lot note: %s")))

(defn delete
  [note & {:as opts}]
  (api/delete (api/path :lot-notes (:id note))
              (add-error-handler opts "Unable to delete lot note: %s")))
