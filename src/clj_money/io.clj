(ns clj-money.io
  (:refer-clojure :exclude [update])
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.io ByteArrayOutputStream]))

(defn read-bytes
  [input]
  (when (nil? input)
    (throw (IllegalArgumentException. "input is required")))

  (with-open [out (ByteArrayOutputStream.)]
    (io/copy input out)
    (.toByteArray out)))

(defn file-name
  [path]
  (last (string/split path (re-pattern java.io.File/separator))))

(defn file-ext
  [path]
  (->> (string/split (file-name path) #"\.")
       rest
       (clojure.string/join ".")))
