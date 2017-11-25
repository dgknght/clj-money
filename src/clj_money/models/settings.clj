(ns clj-money.models.settings
  (:refer-clojure :exclude [get])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [clj-time.core :refer [local-date]]
            [clj-time.format :refer [parse-local-date
                                     unparse-local-date
                                     formatters]]
            [clj-money.models.helpers :refer [with-storage]]
            [clj-money.models.storage :refer [put-setting
                                              get-setting]])
  (:import org.joda.time.LocalDate))

(defmethod print-method LocalDate [this ^java.io.Writer w]
  (doto w
    (.write "#local-date \"")
    (.write (unparse-local-date (:year-month-day formatters) this))
    (.write "\"")))

(defn put
  [storage-spec setting-name setting-value]
  (with-storage [s storage-spec]
    (put-setting s (name setting-name) (prn-str setting-value))))

(defn- parse
  [value]
  (parse-local-date (:year-month-day formatters) value))

(defn get
  [storage-spec setting-name]
  (with-storage [s storage-spec]
    (when-let [value (get-setting s (name setting-name))]
      (read-string value))))
