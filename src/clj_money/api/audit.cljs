(ns clj-money.api.audit
  (:require [clj-money.api :as api :refer [add-error-handler]]))

(defn select
  [entity-type entity-id attr & {:as opts}]
  (api/get (api/path entity-type entity-id :audit)
           {:attr attr}
           (add-error-handler opts "Unable to retrieve audit history: %s")))
