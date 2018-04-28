(ns clj-money.io
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io])
  (:import [java.io ByteArrayOutputStream]))

(defn read-bytes
  [input]
  (when (nil? input)
    (throw (IllegalArgumentException. "input is required")))

  (with-open [out (ByteArrayOutputStream.)]
    (io/copy input out)
    (.toByteArray out)))
