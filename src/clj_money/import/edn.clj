(ns clj-money.import.edn
  (:require [clj-money.import :refer [read-source]]))

(defmethod read-source :edn
  [input]
  (pprint {:read-source input}))


