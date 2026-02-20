(ns clj-money.api.memo-ledger-entries
  (:require [clj-money.api :as api :refer [add-error-handler]]))

(defn select
  [{:memo-ledger-entry/keys [lot] :as criteria} & {:as opts}]
  (api/get (api/path :lots lot :memo-ledger-entries)
           (dissoc criteria :memo-ledger-entry/lot)
           (add-error-handler opts "Unable to retrieve memo entries: %s")))

(defn create
  [{:memo-ledger-entry/keys [lot] :as entry} & {:as opts}]
  (api/post (api/path :lots lot :memo-ledger-entries)
            (dissoc entry :memo-ledger-entry/lot)
            (add-error-handler opts "Unable to create memo entry: %s")))

(defn delete
  [entry & {:as opts}]
  (api/delete (api/path :memo-ledger-entries (:id entry))
              (add-error-handler opts "Unable to delete memo entry: %s")))
