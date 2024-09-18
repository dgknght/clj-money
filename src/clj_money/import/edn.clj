(ns clj-money.import.edn
  (:refer-clojure :exclude [update])
  (:require [clojure.java.io :refer [reader]]
            [clojure.core.async :refer [onto-chan! <!!]]
            [clj-money.import :refer [read-source]])
  (:import [java.util.zip GZIPInputStream]))

(defn- read-input
  [input]
  (-> (GZIPInputStream. input)
      reader
      slurp
      read-string))

(defmethod read-source :edn
  [_ inputs out-chan]
  (<!! (onto-chan!
        out-chan
        (mapcat read-input inputs)
        true)))
