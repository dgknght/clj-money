(ns clj-money.decimal
  (:refer-clojure :exclude [* + - / zero? abs])
  #?(:cljs (:require[dgknght.app-lib.decimal :as decimal])))

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def d
  #?(:clj bigdec
     :cljs decimal/->decimal))

(def zero?
  #?(:clj clojure.core/zero?
     :cljs decimal/zero?))

(def +
  #?(:cljs decimal/+
     :clj clojure.core/+))

(def -
  #?(:cljs decimal/-
     :clj clojure.core/-))

(def *
  #?(:cljs decimal/*
     :clj clojure.core/*))

(defn /
  [n1 n2]
  #?(:cljs (decimal// n1 n2)
     :clj (if *math-context*
            (clojure.core// n1 n2)
            (with-precision 3
              (clojure.core// n1 n2)))))

(defn round
  [n]
  #?(:cljs (decimal/round n)
     :clj (.setScale n 0 java.math.RoundingMode/HALF_UP)))

#?(:cljs (defn abs [n] (decimal/abs n))
   :clj  (defn abs [^java.math.BigDecimal n] (.abs n)))
