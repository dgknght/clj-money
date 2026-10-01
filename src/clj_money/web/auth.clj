(ns clj-money.web.auth
  (:require [buddy.sign.jwt :as jwt]
            [jsonista.core :as json]))

(defn make-token
  "Returns an auth token for the user, signed with the given secret"
  [user secret]
  (jwt/sign {:user-id (:id user)} secret))

(defn read-token
  "Returns the claims of the given auth token, verified with the given
  secret"
  [token secret]
  (jwt/unsign token secret))

(defn make-json-request
  [method uri options]
  (let [req (merge options {:accept :json})
        res (method uri req)]
    (json/read-value (:body res)
                     (json/object-mapper {:decode-key-fn true}))))
