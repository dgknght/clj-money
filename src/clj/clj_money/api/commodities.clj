(ns clj-money.api.commodities
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [ring.util.response :refer [status response header]]
            [cheshire.core :as json]
            [clj-money.api :refer [->response error->response]]
            [clj-money.validation :as validation]
            [clj-money.authorization :refer [authorize]]
            [clj-money.models.commodities :as commodities]
            [clj-money.permissions.commodities]))

(defn index
  [{{entity-id :entity-id} :params}]
  (->response (commodities/search (env :db) {:entity-id entity-id})))

(defn update
  [req]
  (->response {}))

(defn delete
  [req]
  (->response []))
