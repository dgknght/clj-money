(ns clj-money.import.edn
  (:refer-clojure :exclude [update])
  (:require [clojure.java.io :refer [reader]]
            [clojure.core.async :as a]
            [clj-money.import :refer [read-source]])
  (:import [java.util.zip GZIPInputStream]))

(defn- read-input
  [input]
  (-> (GZIPInputStream. input)
      reader
      slurp
      read-string))

(defmethod read-source :edn
  [_ inputs]
  (->> inputs
       (mapcat read-input)
       a/to-chan!))
