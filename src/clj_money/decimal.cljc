(ns clj-money.decimal
   (:refer-clojure :exclude [* + - / zero? abs])
   #?(:cljs (:require [dgknght.app-lib.decimal :as decimal]
                      ["decimal.js" :as Decimal]))
   #?(:clj (:import [java.math BigDecimal MathContext RoundingMode])))

#?(:cljs (extend-protocol IPrintWithWriter
            Decimal
            (-pr-writer [d writer _]
               (write-all writer "#bigdec \"" d "\""))))

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def d
  #?(:clj bigdec
     :cljs decimal/->decimal))

#?(:cljs (defn zero? [n] (decimal/zero? n))
   :clj  (defn zero? [^java.math.BigDecimal n]
           (.equals BigDecimal/ZERO n)))

#?(:cljs (defn + [n1 n2] (decimal/+ n1 n2))
   :clj  (defn + [^java.math.BigDecimal n1
                  ^java.math.BigDecimal n2]
           (.add n1 n2)))

#?(:cljs (defn - [n1 n2] (decimal/- n1 n2))
   :clj  (defn - [^java.math.BigDecimal n1
                  ^java.math.BigDecimal n2]
           (.subtract n1 n2)))

#?(:cljs (defn * [n1 n2] (decimal/* n1 n2))
   :clj  (defn * [^java.math.BigDecimal n1
                  ^java.math.BigDecimal n2]
           (.multiply n1 n2)))

#?(:cljs (defn / [n1 n2] (decimal// n1 n2))
   :clj  (defn / [^java.math.BigDecimal n1
                  ^java.math.BigDecimal n2]
           (.divide n1 n2 (MathContext. 10 RoundingMode/HALF_UP))))

#?(:cljs (defn round
           ([n]
            (decimal/round n))
           ([n places]
            (let [factor (Math/pow 10 places)]
              (/ (round (* factor n))
                 factor))))
   :clj  (defn round
           ([^java.math.BigDecimal n]
            (round n 0))
           ([^java.math.BigDecimal n places]
            (.setScale n places BigDecimal/ROUND_HALF_UP))))

#?(:cljs (defn abs [n] (decimal/abs n))
   :clj  (defn abs [^java.math.BigDecimal n] (.abs n)))
