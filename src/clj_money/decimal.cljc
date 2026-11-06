(ns clj-money.decimal
   (:refer-clojure :exclude [* + - / < <= > >= zero? abs decimal?])
   (:require [clojure.string :as string]
             #?(:clj [clojure.core :as core])
             [dgknght.app-lib.math :as math]
             #?(:cljs [dgknght.app-lib.decimal :as decimal]))
   #?(:clj (:import [java.math BigDecimal MathContext RoundingMode])))

#?(:cljs (extend-protocol IPrintWithWriter
            js/Decimal
            (-pr-writer [d writer _]
               (write-all writer "#clj-money/decimal \"" d "\""))))

(def d
  #?(:clj bigdec
     :cljs decimal/->decimal))

(def decimal?
   #?(:clj core/decimal?
      :cljs decimal/decimal?))

(def zero #?(:clj BigDecimal/ZERO
             :cljs (d 0)))

(def zero? #?(:clj (fn [^BigDecimal n]
                      (.equals BigDecimal/ZERO n))
              :cljs decimal/zero?))

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

#?(:cljs (defn < [n1 n2] (decimal/< n1 n2))
   :clj  (defn < [^java.math.BigDecimal n1
                  ^java.math.BigDecimal n2]
           (core/< n1 n2)))

#?(:cljs (defn > [n1 n2] (decimal/> n1 n2))
   :clj  (defn > [^java.math.BigDecimal n1
                  ^java.math.BigDecimal n2]
           (core/> n1 n2)))

#?(:cljs (defn <= [n1 n2] (decimal/< n1 n2))
   :clj  (defn <= [^java.math.BigDecimal n1
                  ^java.math.BigDecimal n2]
           (core/<= n1 n2)))

#?(:cljs (defn >= [n1 n2] (decimal/> n1 n2))
   :clj  (defn >= [^java.math.BigDecimal n1
                  ^java.math.BigDecimal n2]
           (core/>= n1 n2)))

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

#?(:cljs (def abs decimal/abs)
   :clj  (defn abs [n] (when n (core/abs n))))

(defn parse
   [s]
   (math/eval (string/replace s "," "")))
