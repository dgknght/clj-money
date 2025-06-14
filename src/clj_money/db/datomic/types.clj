(ns clj-money.db.datomic.types
  (:require [clojure.walk :refer [postwalk]]
            [java-time.api :as t]))

(derive java.lang.Integer ::integer)
(derive java.lang.Long ::integer)
(derive java.lang.String ::string)
(derive clojure.lang.PersistentVector ::vector)

(defmulti coerce-id type)

(defmethod coerce-id ::integer [id] id)

(defmethod coerce-id ::string
  [id]
  (Long/parseLong id))

(defmethod coerce-id ::vector
  [v]
  (mapv (fn [x]
          (if (string? x)
            (coerce-id x)
            x))
        v))

(defn ->java-date
  [local-date]
  (t/java-date
    (t/zoned-date-time local-date
                       (t/local-time 0 0 0 0)
                       (t/zone-offset 0 0))))

(defn ->java-dates
  "Given a model or criteria, replace all local-date instances with java dates"
  [m]
  (postwalk (fn [x]
              (if (t/local-date? x)
                (->java-date x)
                x))
            m))
