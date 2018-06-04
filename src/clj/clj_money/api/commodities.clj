(ns clj-money.api.commodities
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [ring.util.response :refer [status response header]]
            [environ.core :refer [env]]
            [cheshire.core :as json]
            [clj-money.validation :as validation]
            [clj-money.authorization :refer [authorize]]
            [clj-money.models.commodities :as commodities]
            [clj-money.permissions.commodities]))

(defn index
  [req]
  [])

(defn update
  [req]
  {})

(defn delete
  [req]
  [])
