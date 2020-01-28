(ns clj-money.api.entities
  (:refer-clojure :exclude [update])
  (:require [clj-money.api :as api]))

(defn get-all
  [success-fn error-fn]
  (api/get-resources (api/path :entities) success-fn error-fn))

(defn create
  [entity success-fn error-fn]
  (api/create-resource (api/path :entities) entity success-fn error-fn))

(defn update
  [entity success-fn error-fn]
  (api/update-resource (api/path :entities (:id entity)) entity success-fn error-fn))

(defn save
  [entity success-fn error-fn]
  (if (:id entity)
    (update entity success-fn error-fn)
    (create entity success-fn error-fn)))

(defn delete
  [entity success-fn error-fn]
  (api/delete-resource (api/path :entities (:id entity))
                       success-fn
                       error-fn))
