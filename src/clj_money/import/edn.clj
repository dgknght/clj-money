(ns clj-money.import.edn
  (:require [clojure.java.io :refer [reader]]
            [clojure.edn :as edn]
            [clojure.core.async :as a]
            [clj-money.dates :as dates]
            [clj-money.decimal :refer [d]]
            [clj-money.import :refer [read-source]])
  (:import [java.util.zip GZIPInputStream]))

(def ^:private edn-readers
  {'clj-money/local-date      dates/local-date
   'clj-money/local-date-time dates/local-date-time
   'clj-money/instant         dates/parse-instant
   'clj-money/decimal         d})

(defn- read-input
  [input]
  (edn/read-string {:readers edn-readers}
                   (-> (GZIPInputStream. input)
                       reader
                       slurp)))

(defmethod read-source :edn
  [_ inputs]
  (->> inputs
       (mapcat read-input)
       a/to-chan!))
