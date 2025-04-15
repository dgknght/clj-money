(ns clj-money.decimal
   (:refer-clojure :exclude [* + - / zero? abs decimal?])
   (:require [clojure.walk :refer [postwalk]]
             [clojure.string :as string]
             [dgknght.app-lib.math :as math]
             #?(:cljs [dgknght.app-lib.decimal :as decimal]))
   #?(:clj (:import [java.math BigDecimal MathContext RoundingMode])))

#?(:cljs (extend-protocol IPrintWithWriter
            js/Decimal
            (-pr-writer [d writer _]
               (write-all writer "#clj-money/decimal \"" d "\""))))

#?(:clj (deftype Decimal [d]
           Object
           (toString [_] (.toString d))))

#?(:clj (defmethod print-method Decimal [this ^java.io.Writer w]
           (doto w
              (.write "#clj-money/decimal \"")
              (.write (.toString this))
              (.write "\""))))

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def d
  #?(:clj bigdec
     :cljs decimal/->decimal))

(def decimal?
   #?(:clj clojure.core/decimal?
      :cljs decimal/decimal?))

(defn wrap-decimals
           "Given a data structure, find all instances of java.Math.BigDecimal and
           replace them with clj-money.decimal/Decimal"
           [m]
           (postwalk (fn [x]
                        (if (decimal? x)
                           #?(:clj (->Decimal x)
                              :cljs (js/Decimal. x))
                           x))
                     m))

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

(def ^:private parsers
   [{:pattern #".*"
     :eval math/eval}
    {:pattern #"(\d+)(?:,(\d+))*"
     :eval #(->> %
                 (drop 1)
                 (string/join "")
                 (d))}])

(defn parse
   [s]
   (some (fn [{:keys [pattern eval]}]
            (when-let [match (re-find pattern s)]
               (eval match)))
         parsers))
