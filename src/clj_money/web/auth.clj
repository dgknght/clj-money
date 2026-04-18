(ns clj-money.web.auth
  (:require [buddy.sign.jwt :as jwt]
            [clj-money.config :refer [env]]
            [jsonista.core :as json]))

(defn make-token
  [user]
  (jwt/sign {:user-id (:id user)} (env :secret)))

(defn make-json-request
  [method uri options]
  (let [req (merge options {:accept :json})
        res (method uri req)]
    (json/read-value (:body res)
                     (json/object-mapper {:decode-key-fn true}))))
