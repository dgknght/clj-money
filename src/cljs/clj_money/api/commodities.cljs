(ns clj-money.api.commodities
  (:refer-clojure :exclude [update count])
  (:require [clj-money.api :as api]))

(defn count
  [entity-id success-fn error-fn]
  (api/get (api/path :entities entity-id :commodities :count)
           success-fn
           error-fn))

(defn get-all
  ([entity-id success-fn error-fn]
   (get-all entity-id {} success-fn error-fn))
  ([entity-id criteria success-fn error-fn]
   (api/get-resources (api/path :entities entity-id :commodities)
                      criteria
                      success-fn
                      error-fn)))

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

(defn save
  [commodity success-fn error-fn]
  (if (:id commodity)
    (update commodity success-fn error-fn)
    (create commodity success-fn error-fn)))

(defn delete
  [commodity success-fn error-fn]
  (api/delete-resource (api/path :commodities (:id commodity))
                       success-fn
                       error-fn))
