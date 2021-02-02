(ns clj-money.decimal
  (:refer-clojure :exclude [+ - * / zero?]))

(defn ->decimal
  [value]
  (when value
    (js/window.Decimal. value)))

(defn abs
  [value]
  (when value
    (.abs (->decimal value))))

(defn zero []
  (->decimal 0))

(defn zero?
  [value]
  (when value
    (.isZero (->decimal value))))

(defn +
  [v1 v2]
  (cond
    (and (nil? v1)
         (nil? v2)) nil
    (nil? v1)       v2
    (nil? v2)       v1
    :else           (.plus (->decimal v1) v2)))

(defn -
  [v1 v2]
  (when-not (or (nil? v1)
                (nil? v2))
    (.minus (->decimal v1) v2)))

(defn *
  [v1 v2]
  (.times (->decimal v1) v2))

(defn /
  [v1 v2]
  (.div (->decimal v1) v2))

(defn sum
  [coll]
  (->> coll
       (filter identity)
       (reduce + 0M )))

(defn equal?
  [d1 d2]
  (.equals (->decimal d1) d2))
