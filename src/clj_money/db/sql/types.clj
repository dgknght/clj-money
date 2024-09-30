(ns clj-money.db.sql.types
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [multi-money.util :as utl]))

(derive java.lang.Integer ::integer)
(derive java.lang.Long ::integer)
(derive java.lang.String ::string)
(derive java.util.UUID ::uuid)
(derive clojure.lang.PersistentVector ::vector)
(derive ::integer ::id)
(derive ::uuid ::id)

(defn temp-id
  "Generates a new temporary id"
  []
  (str "temp-" (random-uuid)))

(defn temp-id?
  "Given a model or an id, returns true if the model has a temporary
  id or if the specified id is a temporary id"
  [id-or-model]
  (let [id (utl/->id id-or-model)]
    (and (string? id)
         (string/starts-with? id "temp-"))))

(defmulti coerce-id type)

(defmethod coerce-id ::id [id] id)

(defmethod coerce-id ::string
  [s]
  (cond
    (temp-id? s)            s
    (re-find #"^[0-9]+$" s) (parse-long s)
    :else                   (java.util.UUID/fromString s)))

(defmethod coerce-id ::vector
  [v]
  (mapv (fn [x]
          (if (string? x)
            (coerce-id x)
            x))
        v))
