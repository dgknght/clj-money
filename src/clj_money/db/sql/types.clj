(ns clj-money.db.sql.types
  (:require [clojure.pprint :refer [pprint]]
            [cheshire.core :as json]
            [next.jdbc.result-set :as rs]
            [next.jdbc.prepare :as p]
            [next.jdbc.date-time]
            [clj-money.util :refer [temp-id?]])
  (:import org.postgresql.util.PGobject
           [java.sql Array PreparedStatement]))

(derive java.lang.Integer ::integer)
(derive java.lang.Long ::integer)
(derive java.lang.String ::string)
(derive java.util.UUID ::uuid)
(derive clojure.lang.PersistentVector ::vector)
(derive ::integer ::id)
(derive ::uuid ::id)

(next.jdbc.date-time/read-as-local)

(extend-protocol rs/ReadableColumn
  Array
  (read-column-by-label [^Array v _] (vec (.getArray v)))
  (read-column-by-index [^Array v _ _] (vec (.getArray v))))

(extend-protocol p/SettableParameter
  clojure.lang.Keyword
  (set-parameter [^clojure.lang.Keyword k ^PreparedStatement s ^long i]
    (.setObject s i (name k))))

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

(defn ->json
  [x]
  (when x
    (doto (PGobject.)
      (.setType "jsonb")
      (.setValue (json/generate-string x)))))

(defn json->map
  [^org.postgresql.util.PGobject x & {:keys [key-fn] :or {key-fn true}}]
  (when x
    (json/parse-string (.getValue x) key-fn)))
