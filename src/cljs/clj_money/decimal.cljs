(ns clj-money.decimal
  (:refer-clojure :exclude [+ *]))

(defn ->decimal
  [value]
  (when value
    (js/window.Decimal. value)))

(defn zero []
  (->decimal 0))

(defn +
  [v1 v2]
  (cond
    (and (nil? v1)
         (nil? v2)) nil
    (nil? v1)       v2
    (nil? v2)       v1
    :else           (.plus (->decimal v1) v2)))

(defn *
  [v1 v2]
  (.times (->decimal v1) v2))
