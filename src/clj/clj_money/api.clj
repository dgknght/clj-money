(ns clj-money.api
  (:refer-clojure :exclude [update])
  (:require [ring.util.response :refer [status response header]]
            [environ.core :refer [env]]
            [cheshire.core :as json]))

(defn ->response
  ([value] (->response value 200))
  ([value status-code]
   (-> value
       json/generate-string
       response
       (header "Content-Type" "application/json")
       (status status-code))))

(defn error->response
  [error safe-error-message]
  (->response
    (if (env :show-error-messages?)
      {:message (.getMessage error)
       :type (.getName (.getClass error))
       :stack (.getStackTrace error)}
      {:message safe-error-message})
    500))
