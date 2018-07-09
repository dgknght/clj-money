(ns clj-money.api.commodities
  (:refer-clojure :exclude [update])
  (:require [clj-money.api :as api]))

(defn get-all
  [entity-id success-fn error-fn]
  (api/get-resources (api/path :entities entity-id :commodities)
                     success-fn
                     error-fn))

(defn get-one
  [id success-fn error-fn]
  (api/get-resources (api/path :commodities id) success-fn error-fn))

(defn create
  [commodity success-fn error-fn]
  (api/create-resource (api/path :entities (:entity-id commodity) :commodities)
                       commodity
                       success-fn
                       error-fn))

(defn update
  [commodity success-fn error-fn]
  (api/update-resource (api/path :commodities (:id commodity))
                       commodity
                       success-fn
                       error-fn))

(defn delete
  [commodity success-fn error-fn]
  (api/delete-resource (api/path :commodities (:id commodity))
                       success-fn
                       error-fn))
