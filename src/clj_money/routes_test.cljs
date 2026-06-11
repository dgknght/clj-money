(ns clj-money.routes-test
  (:require [cljs.test :refer [deftest are]]
            [clj-money.routes :as rts]))

(deftest identity-a-server-path
  (are [route] (rts/server-path? route)
       "/api/accounts"
       "/oapi/users/authenticate"
       "/auth/google/done"
       "/app/images/101"))

(deftest identity-a-non-server-path
  (are [route] (not (rts/server-path? route))
       "/accounts"))

(deftest identify-a-public-path
  (are [route] (rts/public-path? route)
       "/login"
       "/setup"
       "/accept-invitation/abc123"
       "/decline-invitation/abc123"))

(deftest identity-a-non-public-path
  (are [route] (not (rts/public-path? route))
       "/accounts"))
