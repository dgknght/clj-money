(ns clj-money.import.edn
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.java.io :refer [reader]]
            [clj-money.import :refer [read-source]])
  (:import [java.util.zip GZIPInputStream]))

(defmethod read-source :edn
  [_ input]
  (-> (GZIPInputStream. input)
      reader
      slurp
      read-string))


