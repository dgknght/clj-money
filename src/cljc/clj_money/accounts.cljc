(ns clj-money.accounts
  (:refer-clojure :exclude [+ - * / abs])
  (:require [clojure.string :as string]
            [dgknght.app-lib.models :as models]
            [dgknght.app-lib.web :refer [format-decimal]]
            #?(:cljs [dgknght.app-lib.decimal :as decimal])
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])))

(def account-types
  "The list of valid account types in standard presentation order"
  [:asset :liability :equity :income :expense])

(defn +
  [n1 n2]
  #?(:cljs (decimal/+ n1 n2)
     :clj (clojure.core/+ n1 n2)))

(defn -
  [n1 n2]
  #?(:cljs (decimal/- n1 n2)
     :clj (clojure.core/- n1 n2)))

(defn *
  [n1 n2]
  #?(:cljs (decimal/* n1 n2)
     :clj (clojure.core/* n1 n2)))

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

(defn- eval-children
  [{:keys [children] :as account}]
  (let [children-value (->> children
                            (mapcat (juxt :value :children-value))
                            (filter identity)
                            (reduce + 0M))]
    (assoc account
           :children-value children-value
           :total-value (+ children-value
                           (or (:value account) 0M))
           :has-children? true)))

(defn nest
  ([accounts] (nest {} accounts))
  ([{:keys [types]
     :or {types account-types}}
    accounts]
   (let [by-type (->> accounts
                      (map #(assoc % :total-value (:value %)))
                      (group-by :type))]
     (mapv #(hash-map :type %
                      :accounts (models/nest
                                  {:decorate-parent-fn eval-children}
                                  (get-in by-type [%])))
           types))))

(defn unnest
  [types]
  (->> types
       (mapcat :accounts)
       models/unnest))

(defn left-side?
  "Returns truthy if the specified account is asset or expense, falsey if anything else"
  [account]
  {:pre [(:type account)]}

  (#{:asset :expense} (:type account)))

(defn- polarizer
  [transaction-item account]
  (* (if (left-side? account) 1 -1)
     (if (= :debit (:action transaction-item)) 1 -1)))

(defn polarize-quantity
  "Adjusts the polarity of a quantity as appropriate given
  a transaction item action and the type of the associated account"
  [transaction-item account]
  (* (:quantity transaction-item account)
     (polarizer transaction-item account)))

(defn derive-action
  "Given a quantity (either positve or negative) and an
  account, returns the appropriate action (debit or credit)"
  [quantity account]
  (if (< quantity 0)
    (if (left-side? account)
      :credit
      :debit)
    (if (left-side? account)
      :debit
      :credit)))

(defn derive-item
  "Given a quantity and an account, returns a transaction item
  with appropriate attributes"
  [quantity account]
  {:quantity (abs quantity)
   :account-id (:id account)
   :action (derive-action quantity account)})

(defn ->criteria
  ([account] (->criteria account {}))
  ([account {:keys [date-field earliest-date latest-date]
             :or {date-field :transaction-date}}]
   (if (sequential? account)
     {:account-id (set (map :id account))
      :transaction-date [:between
                         (or (->> account
                                  (map :earliest-transaction-date)
                                  (filter identity)
                                  (sort-by t/before?)
                                  first)
                             earliest-date)
                         (or (->> account
                                  (map :latest-transaction-date)
                                  (filter identity)
                                  (sort-by t/after?)
                                  first)
                             latest-date)]}
     {:account-id (:id account)
      date-field [:between
                  (:earliest-transaction-date account)
                  (:latest-transaction-date account)]})))

(defn- match-path?
  [path terms]
  (loop [t (first terms) tt (rest terms) p (first path) pp (rest path)]
    (when (and t p)
      (if (string/includes? p t)
        (if (seq tt)
          (recur (first tt) (rest tt) (first pp) (rest pp))
          true)
        (recur t tt (first pp) (rest pp))))))

(defn find-by-path
  [term accounts]
  (filter #(match-path? (map string/lower-case (:path %))
                        (map string/lower-case (string/split term #"/|:")))
          accounts))

(defn user-tagged?
  [{:keys [user-tags]} tag]
  (contains? user-tags tag))

(defn format-quantity
  [quantity {:keys [commodity]}]
  (format-decimal quantity
                  (if (= "USD" (:symbol commodity))
                    {:fraction-digits 2}
                    {:fraction-digits 4})))

(defn- ->allocation-rec
  [{:keys [target-percentage account] :as m} working-total]
  (let [percentage (/ target-percentage 100M)
        target-value (* working-total
                        percentage)
        current-value (:value account)
        current-percentage (/ current-value working-total)
        raw-adj-value (- target-value
                         current-value)
        adj-value (if (< (abs raw-adj-value) 100M)
                    0M
                    (* 100M
                       (round (/ raw-adj-value
                                 100M))))]
    (assoc m
           :current-value current-value
           :current-percentage current-percentage
           :target-value target-value
           :adj-value adj-value)))

(defn allocate
  ([account find-account-fn]
   (allocate account find-account-fn {}))
  ([{:keys [allocations total-value value]} find-account-fn {:keys [cash withdrawal]}]
   (let [cash-withheld (or cash value)
         withdrawal (or withdrawal 0M)
         working-total (- total-value (+ cash-withheld withdrawal))
         result (->> allocations
                     (map (comp #(->allocation-rec % working-total)
                                (fn [[id target-percentage]]
                                  {:account (find-account-fn id)
                                   :target-percentage target-percentage})))
                     (sort-by (comp abs :adj-value) >)
                     (into []))
         net (->> result
                  (map :adj-value)
                  (reduce + withdrawal))]
     (if (zero? net)
       result
       (update-in result [0 :adj-value] - net)))))
