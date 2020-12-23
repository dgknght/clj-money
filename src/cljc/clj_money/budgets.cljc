(ns clj-money.budgets
  (:require #?(:clj [clj-time.core :as t]
               :cljs [cljs-time.core :as t])
            #?(:clj [clj-time.format :as tf]
               :cljs [cljs-time.format :as tf])
            #?(:clj [clj-time.coerce :as tc]
               :cljs [cljs-time.coerce :as tc])
            #?(:cljs [clj-money.decimal :as decimal])))

(def periods #{:month :quarter :year})

(defmulti period-description
  (fn [_ budget]
    (:period budget)))

(defmethod period-description :month
  [index {:keys [start-date]}]
  (tf/unparse (tf/formatter "MMM YYYY")
              (tc/to-date-time (t/plus start-date (t/months index)))))

(defmethod period-description :quarter
  [index {:keys [start-date]}]
  (str "Q" (inc index) " " (t/year start-date)))

(defn- sum
  [coll]
  #?(:clj (reduce + coll)
     :cljs (decimal/sum coll)))

(defn- render-items
  [items caption filter-fn]
  (let [rendered-items (->> items
                            (filter filter-fn)
                            (sort-by (comp :path :account))
                            (map (fn [item]
                                   {:item (dissoc item :account) ; TODO: should I remove this here? It simplifies testing, but is it better?
                                    :caption (-> item :account :path)
                                    :total (sum (:periods item))})))]
    {:caption caption
     :items rendered-items
     :total (->> rendered-items
                 (map :total)
                 sum)
     :periods (->> rendered-items
                   (map (comp :periods :item))
                   (apply interleave)
                   (partition (count rendered-items))
                   (map #(sum %)))}))

(defn- render-income
  [items]
  (render-items items "Income" #(= :income (-> % :account :type))))

(defn- render-expense
  [items]
  (render-items items "Expenses" #(not= :income (-> % :account :type))))

(defn render
  "Given a budget and a fn to look up accounts by id, 
  returns a structure with summarized totals by infow and outflow"
  [budget find-account]
  (let [with-accounts (update-in budget [:items] (fn [items]
                                                   (map #(assoc % :account (find-account (:account-id %)))
                                                        items)))
        income (render-income (:items with-accounts))
        expense (render-expense (:items with-accounts))
        net-periods (->> [income expense]
                         (map :periods)
                         (apply interleave)
                         (partition 2)
                         (map #(apply - %)))
        net {:caption "Net"
             :periods net-periods
             :total (sum net-periods)}]
    [income
     expense
     net]))
