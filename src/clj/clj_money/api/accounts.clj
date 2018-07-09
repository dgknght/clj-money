(ns clj-money.api.accounts
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [ring.util.response :refer [status response header]]
            [clj-money.api :refer [->response
                                   error->response
                                   delete-resource]]
            [clj-money.validation :as validation]
            [clj-money.authorization :refer [authorize
                                             tag-resource]]
            [clj-money.models.accounts :as accounts]
            [clj-money.permissions.accounts]))

(defn index
  [{{entity-id :entity-id} :params}]
  (->response (accounts/search (env :db) {:entity-id entity-id})))

(defn get-account
  [{{id :id} :params}]
  (->response (accounts/find-by-id (env :db) id)))
