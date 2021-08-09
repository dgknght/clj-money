(ns clj-money.accounts
  (:require [clojure.string :as string]
            [dgknght.app-lib.models :as models]
            [clj-money.util :refer [abs]]
            #?(:cljs [dgknght.app-lib.decimal :as decimal])
            #?(:clj [clj-time.coerce :as tc]
               :cljs [cljs-time.coerce :as tc])))

(def account-types
  "The list of valid account types in standard presentation order"
  [:asset :liability :equity :income :expense])

(defn plus
  [n1 n2]
  #?(:cljs (decimal/+ n1 n2)
     :clj (+ n1 n2)))

(defn- eval-children
  [{:keys [children] :as account}]
  (let [children-value (->> children
                            (mapcat (juxt :value :children-value))
                            (filter identity)
                            (reduce plus 0M))]
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

(def right-side? (comp left-side?))

(defn- polarizer
  [transaction-item account]
  (* (if (left-side? account) 1 -1)
     (if (= :debit (:action transaction-item)) 1 -1)))

(defn polarize-item
  [transaction-item account]
  (let [polarizer (polarizer transaction-item account)]
    (assoc transaction-item
           :polarized-quantity (* polarizer (:quantity transaction-item))
           :polarized-value    (* polarizer (:value transaction-item)))))

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
                                  (sort-by tc/to-long)
                                  first)
                             earliest-date)
                         (or (->> account
                                  (map :latest-transaction-date)
                                  (filter identity)
                                  (sort-by tc/to-long >)
                                  first)
                             latest-date)]}
     {:account-id (:id account)
      date-field [:between
                  (:earliest-transaction-date account)
                  (:latest-transaction-date account)]})))

(defn find-by-path
  [term accounts]
  (let [term (string/lower-case term)]
    (filter #(->> (get-in % [:path])
                  (map string/lower-case)
                  (some (fn [segment]
                          (string/includes? segment term))))
            accounts)))

(defn user-tagged?
  [{:keys [user-tags]} tag]
  (contains? user-tags tag))
