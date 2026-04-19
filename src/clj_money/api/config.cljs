(ns clj-money.api.config
  (:require [clj-money.api :as api :refer [add-error-handler]]))

(defn fetch
  [& {:as opts}]
  (api/get "/oapi/config"
           {}
           (add-error-handler opts "Unable to retrieve application config: %s")))
