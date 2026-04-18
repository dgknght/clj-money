(ns clj-money.api.audit
  (:require [clj-money.api :as api :refer [add-error-handler]]))

(defn select
  [{:keys [id] :as entity} attr & {:as opts}]
  {:pre [(:id entity)]}
  (api/get (api/path :entities id :audit)
           {:attr (str (namespace attr) "/" (name attr))}
           (add-error-handler opts "Unable to retrieve audit history: %s")))
