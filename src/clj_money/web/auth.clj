(ns clj-money.web.auth
  (:require [buddy.sign.jwt :as jwt]
            [java-time.api :as t]
            [jsonista.core :as json]))

(def ^:private token-lifetime (t/days 7))

(defn make-token
  "Returns an auth token for the user, signed with the given secret,
  that expires after the token lifetime"
  [user secret]
  (jwt/sign {:user-id (:id user)
             :exp (.getEpochSecond (t/plus (t/instant) token-lifetime))}
            secret))

(defn read-token
  "Returns the claims of the given auth token, verified with the given
  secret, or nil if the token is invalid or has expired"
  [token secret]
  (try
    (jwt/unsign token secret {:now (.getEpochSecond (t/instant))})
    (catch clojure.lang.ExceptionInfo e
      (when-not (= :validation (:type (ex-data e)))
        (throw e)))))

(defn make-json-request
  [method uri options]
  (let [req (merge options {:accept :json})
        res (method uri req)]
    (json/read-value (:body res)
                     (json/object-mapper {:decode-key-fn true}))))
