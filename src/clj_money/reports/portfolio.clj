(ns clj-money.reports.portfolio
  (:require [java-time.api :as t]
            [dgknght.app-lib.core :refer [index-by]]
            [clj-money.entities :as entities]
            [clj-money.util :refer [id=]]
            [clj-money.dates :as dates]
            [clj-money.accounts :refer [nest
                                        system-tagged?]]
            [clj-money.reports :as rpt :refer [sum]]))

(declare aggregate-portfolio-account)

(defn- aggregate-portfolio-commodity-accounts
  [{:account/keys [children]} depth]
  (mapcat #(aggregate-portfolio-account % :depth (inc depth))
          children))

(defn- aggregate-portfolio-lots
  [{:account/keys [lots]} depth]
  (map (fn [{:keys [id]
              :lot/keys [purchase-date
                         shares-purchased
                         shares-owned
                         value
                         cost-basis
                         gain]}]
         {:id id
          :report/caption (dates/format-local-date purchase-date)
          :report/depth (inc depth)
          :report/style :data
          :report/shares-purchased shares-purchased
          :report/shares-owned shares-owned
          :report/current-value value
          :report/cost-basis cost-basis
          :report/gain-loss gain
          :report/gain-loss-percent (with-precision 3
                                      (/ gain cost-basis))})
       lots))

(defn- format-commodity
  [{:commodity/keys [name symbol]}]
  (format "%s (%s)" name symbol))

(defn- aggregate-portfolio-account
  [{:account/keys [name
                   shares-purchased
                   shares-owned
                   total-value
                   cost-basis
                   value
                   commodity
                   gain]
    :as account}
   & {:keys [depth] :or {depth 0}}]
  {:pre [(:account/total-value account)
         (:account/value account)]}
  (let [cost-basis (or cost-basis value 0M)
        gain (or gain (- value cost-basis))
        shared {:id (:id account)
                :report/caption name
                :report/depth depth
                :report/cost-basis cost-basis
                :report/gain-loss gain
                :report/gain-loss-percent (if (< 0M cost-basis)
                                            (with-precision 3
                                              (/ gain cost-basis))
                                            0M)
                :report/shares-purchased shares-purchased
                :report/shares-owned shares-owned
                :report/current-value total-value}]
    (if (system-tagged? account :trading)
      (cons (merge shared
                   {:report/style :header
                    :report/caption name})
            (cons {:id (str "cash-" (:id account))
                   :report/caption "Cash"
                   :report/style :subheader
                   :report/depth (inc depth)
                   :report/current-value value
                   :report/cost-basis value
                   :report/gain-loss 0M}
                  (aggregate-portfolio-commodity-accounts account depth)))
      (cons (merge shared
                   {:report/style :subheader
                    :report/caption (format-commodity commodity)})
            (aggregate-portfolio-lots account depth)))))

(defn- aggregate-portfolio-by-account
  [accounts]
  (->> accounts
       nest
       (mapcat :accounts)
       (mapcat aggregate-portfolio-account)))

(defn- aggregate-portfolio-commodity
  [[_commodity-id accounts]]
  (let [commodity (:account/commodity (first accounts))
        current-value (sum :account/value accounts)]
    (if (:commodity/default? commodity)
      (cons {:id (:id commodity)
             :report/caption (format-commodity commodity)
             :report/style :header
             :report/current-value current-value
             :report/cost-basis current-value
             :report/gain-loss 0M
             :report/gain-loss-percent 0.0M}
            (map (fn [account]
                   {:id (:id account)
                    :report/caption (:account/name account)
                    :report/style :data
                    :report/current-value (:account/value account)})
                 accounts))
      (let [cost-basis (sum :account/cost-basis accounts)
            gain (- current-value cost-basis)]
        (cons {:id (:id commodity)
               :report/caption (format-commodity commodity)
               :report/style :header
               :report/shares-purchased (sum :account/shares-purchased accounts)
               :report/shares-owned (sum :account/shares-owned accounts)
               :report/current-value current-value
               :report/cost-basis cost-basis
               :report/gain-loss gain
               :report/gain-loss-percent (with-precision 3
                                           (/ gain cost-basis))}
              (->> accounts
                   (mapcat :account/lots)
                   (sort-by :lot/purchase-date t/after?)
                   (map (fn [{:keys [id]
                              :lot/keys [shares-purchased
                                         shares-owned
                                         cost-basis
                                         gain
                                         purchase-date
                                         value]}]
                          {:id id
                           :report/caption (dates/format-local-date purchase-date)
                           :report/style :data
                           :report/shares-purchased shares-purchased
                           :report/shares-owned shares-owned
                           :report/cost-basis cost-basis
                           :report/current-value value
                           :report/gain-loss gain
                           :report/gain-loss-percent (with-precision 3
                                                       (/ gain cost-basis))}))))))))

(defn- commodity->sortable
  [{:commodity/keys [name]}]
  (if (= "Cash" name)
    (str "* " name)
    name))

(defn- aggregate-portfolio-by-commodity
  [accounts]
  (->> accounts
       (group-by :account/commodity)
       (sort-by (comp commodity->sortable first))
       (mapcat aggregate-portfolio-commodity)))

(defn- add-seqs
  [s1 s2]
  (->> s1
       (interleave s2)
       (partition 2)
       (map (partial apply +))))

(defn- append-portfolio-total
  [records]
  (let [[cost-basis current-value] (->> records
                                        (filter #(= :header (:report/style %)))
                                        (map (juxt :report/cost-basis
                                                   :report/current-value))
                                        (reduce add-seqs [0M 0M]))
        gain-loss (- current-value cost-basis)]
    (conj (vec records)
          {:report/style :summary
           :report/caption "Total"
           :report/current-value current-value
           :report/cost-basis cost-basis
           :report/gain-loss gain-loss
           :report/gain-loss-percent (if (< 0M cost-basis)
                                       (with-precision 3
                                         (/ gain-loss cost-basis))
                                       0M)})))

(defn report
  "Returns a portfolio report based on the options:
    entity    - The entity for which a report is to be generated (required)
    as-of     - The date for which a report is to be generated. Defaults to today
    aggregate - The method, :by-commodity or :by-account, defaults to :by-commodity"
  [{:keys [entity] :as options}]
  {:pre [(:entity options)]}
  (let [accounts (entities/select {:account/entity entity
                                   :account/type :asset
                                   #_:account/system-tags #_[:&& #{:trading :tradable}]})
        commodities (->> (entities/select {:commodity/entity entity})
                         (map (fn [c]
                                (if (id= c
                                         (get-in entity [:entity/settings
                                                         :settings/default-commodity]))
                                  (assoc c
                                         :commodity/name "Cash"
                                         :commodity/default? true)
                                  c)))
                         (index-by :id))
        aggregate (case (:aggregate options)
                    :by-account aggregate-portfolio-by-account
                    :by-commodity aggregate-portfolio-by-commodity
                    (throw (IllegalArgumentException. (str "Unknown aggregate type: " (:aggregate options)))))]
    (->> accounts
         (filter (some-fn (system-tagged? :tradable)
                          (system-tagged? :trading))) ; TODO: Make the system tag query above work
         (map #(update-in % [:account/commodity] (comp commodities :id)))
         (map (fn [account]
                (if (system-tagged? account :trading)
                  (dissoc account :account/parent)
                  account)))
         (rpt/valuate-accounts options)
         (aggregate)
         (append-portfolio-total))))
