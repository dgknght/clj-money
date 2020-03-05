(ns clj-money.api.test-helper
  (:require [ring.mock.request :as req]
            [clj-money.web.auth :as auth]))

(defn add-auth
  [req user]
  {:pre [user]}
  (req/header req "Authorization" (str "Bearer " (auth/make-token user))))
