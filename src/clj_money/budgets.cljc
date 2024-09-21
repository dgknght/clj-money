(ns clj-money.budgets
  (:require [clojure.string :as string]
            [clojure.set :refer [intersection]]
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            [dgknght.app-lib.inflection :refer [title-case]]
            [clj-money.accounts :as acts]
            [clj-money.transactions :as txns]
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            #?(:cljs [cljs-time.format :as tf])
            #?(:cljs [cljs-time.coerce :as tc])
            #?(:cljs [dgknght.app-lib.decimal :as decimal])))

(def periods #{:month :quarter :year})

(defmulti period-description
  (fn [_ budget]
    (:period budget)))

(defmethod period-description :month
  [index {:keys [start-date]}]
  #?(:clj (t/format (t/formatter "MMM YYYY") (t/plus start-date (t/months index)))
     :cljs (tf/unparse (tf/formatter "MMM YYYY")
              (tc/to-date-time (t/plus start-date (t/months index))))))

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
                                    :caption (string/join "/" (-> item :account :path))
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

(defn- income?
  [{:keys [account]}]
  (= :income (:type account)))

; For the purposes of a budget, anything that is not income is outflow
(def expense? (complement income?))

(defn- render-income
  [items]
  (render-items items "Income" income?))

(defn- render-expense
  [items]
  (render-items items "Expenses" expense?))

(defn- item-tagged?
  [tag {:keys [account]}]
  (acts/user-tagged? account tag))

(defn- item-not-tagged?
  [tags {:keys [account]}]
  (empty? (intersection tags (:user-tags account))))

(defn- subtract-periods
  [& groups]
  (->> groups
       (map :periods)
       (apply interleave)
       (partition 2)
       (map #(apply - %))))

(defn- append
  [item coll]
  (concat coll
          [item]))

(defn- process-tagged-items
  [items result {:keys [caption pred summary-caption]}]
  (let [group (render-items items caption pred)
        net-periods (subtract-periods (last result) group)]
    (if (seq (:items group))
      (concat result
              [group
               {:caption summary-caption
                :periods net-periods
                :total (sum net-periods)}])
      result)))

(defn- render-tagged
  [items tags]
  (->> tags
       (map (fn [tag]
              {:caption (str (title-case tag) " Expenses")
               :pred (every-pred expense?
                                 (partial item-tagged? tag))
               :summary-caption (str "Available After " (title-case tag))}))
       (append {:caption "Uncategorized"
                :pred (every-pred expense?
                                  (partial item-not-tagged? (set tags)))
                :summary-caption "Net"})
       (reduce (partial process-tagged-items items)
               [(render-income items)])))

(defn- render-untagged
  [items]
  (let [income (render-income items)
        expense (render-expense items)
        net-periods (subtract-periods income expense)
        net {:caption "Net"
             :periods net-periods
             :total (sum net-periods)}]
    [income
     expense
     net]))

(defn render
  "Given a budget and a fn to look up accounts by id,
  returns a structure with summarized totals by inflow and outflow"
  [{:keys [items]} {:keys [find-account tags] :as options}]
  {:pre [(:find-account options)]}

  (let [items (map #(assoc % :account (find-account (:account-id %)))
                   items)]
    (if (seq tags)
      (render-tagged items tags)
      (render-untagged items))))

(defn- ->budget-item
  [[account-id tran-items]
   {:keys [period]}
   start-date
   end-date]
  {:account-id account-id
   :periods (->> tran-items
                 (txns/summarize-items {:interval-type period
                                        :interval-count 1
                                        :start-date start-date
                                        :end-date end-date})
                 (map :quantity))})

(defn create-items-from-history
  [budget start-date end-date trx-items]
  (->> trx-items
       (group-by :account-id)
       (map #(->budget-item % budget start-date (t/minus end-date (t/days 1))))))
