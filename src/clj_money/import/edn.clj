(ns clj-money.import.edn
  (:require [clojure.java.io :refer [reader]]
            [clojure.edn :as edn]
            [clojure.core.async :as a]
            [clj-money.import :refer [read-source]])
  (:import [java.util.zip GZIPInputStream]))

(defn- read-input
  [input]
  (-> (GZIPInputStream. input)
      reader
      slurp
      edn/read-string))

(defmethod read-source :edn
  [_ inputs]
  (->> inputs
       (mapcat read-input)
       a/to-chan!))
