(ns clj-money.api.users
  (:refer-clojure :exclude [find])
  (:require [compojure.core :refer [defroutes GET]]
            [clj-money.api :as api]))

(defn find
  [{:keys [authenticated]}]
  (api/->response authenticated))

(defroutes routes
  (GET "/api/users/me" req (find req)))
