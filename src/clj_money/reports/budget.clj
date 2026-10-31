(ns clj-money.reports.budget
  (:require [clojure.set :refer [intersection]]
            [clojure.string :as string]
            [java-time.api :as t]
            [dgknght.app-lib.inflection :refer [humanize]]
            [clj-money.entities :as entities]
            [clj-money.dates :as dates :refer [earliest]]
            [clj-money.accounts :refer [nest
                                        unnest
                                        left-side?]]
            [clj-money.budgets :as budgets]
            [clj-money.reports :as rpt :refer [sum]]))

(defn- ->budget-report-record
  [account budget {:keys [period-count]}]
  (let [item (budgets/find-item-by-account budget account)
        budget-amount (if item
                        (reduce + 0M (->> item
                                          :budget-item/periods
                                          (take period-count)))
                        0M) ; TODO only total the periods up to and including the as-of date
        actual-amount (or (:account/value account) 0M)
        difference (if (left-side? account)
                     (- budget-amount actual-amount)
                     (- actual-amount budget-amount))]
    (with-precision 10
      (-> account
          (select-keys [:id
                        :account/user-tags
                        :account/parent])
          (merge #:report{:caption (if (seq (:account/path account))
                                     (string/join "/" (:account/path account))
                                     (:account/name account))
                          :style :data
                          :budget budget-amount
                          :actual actual-amount
                          :difference difference
                          :percent-difference (when (not= 0M budget-amount)
                                                (with-precision 5
                                                  (/ difference budget-amount)))
                          :actual-per-period (with-precision 5
                                               (/ actual-amount period-count))})))))

(defn- budget-group-header
  [period-count header-key records]
  (let [[budget actual diff] (map #(sum % records)
                                  [:report/budget
                                   :report/actual
                                   :report/difference])
        ; If all items are grouped by Untagged, then just ignore the tag layer
        records (if (and (= 1 (count records))
                         (= "Untagged" (:report/caption (first records))))
                  (-> records first :report/items)
                  records)]
    (with-precision 10
      #:report{:caption (humanize header-key)
               :style :header
               :budget budget
               :actual actual
               :difference diff
               :percent-difference (when-not (zero? budget)
                                     (/ diff budget))
               :actual-per-period  (/ actual period-count)
               :items records})))

(defn- append-roll-up
  [{:report/keys [budget actual difference] :as record} children period-count]
  (let [b (+ budget (sum :report/budget children))
        a (+ actual (sum :report/actual children))
        d (+ difference (sum :report/difference children))]
    (assoc record
           :report/roll-up #:report{:budget b
                                    :actual a
                                    :difference d
                                    :percent-difference (when-not (= 0M b)
                                                          (with-precision 10
                                                            (/ d b)))
                                    :actual-per-period (with-precision 10
                                                         (/ a period-count))})))

(defn- roll-up*
  [record depth records-map period-count]
  (let [children (mapcat #(roll-up* % (inc depth) records-map period-count)
                         (get-in records-map [(:id record)] []))
        r (assoc record :report/depth depth)]
    (if (seq children)
      (conj children (append-roll-up r children period-count))
      [r])))

(defn- roll-up
  [period-count records]
  (let [ids (set (map :id records))
        records-map (reduce (fn [m r]
                              (update-in m [(:id (:account/parent r))] (fnil conj []) r))
                            {}
                            records)]
    (->> records
         (remove #(contains? ids (:id (:account/parent %))))
         (mapcat #(roll-up* % 0 records-map period-count)))))

(defn- zero-budget-report-item?
  [item]
  (->> [[:report/actual]
        [:report/budget]
        [:report/roll-up :report/actual]
        [:report/roll-up :report/budget]]
       (map #(get-in item % 0M))
       (every? zero?)))

(defn- process-tag-group
  [[tag accounts] {:keys [period-count budget] :as options}]
  (budget-group-header period-count
                       tag
                       (->> accounts
                            (map #(->budget-report-record % budget options))
                            (sort-by :report/difference)
                            (roll-up period-count)
                            (map #(dissoc % :account/parent))
                            (remove zero-budget-report-item?))))

(defn- group-by-tags
  [tags accounts]
  (let [tagged (->> tags
                    (map (fn [tag]
                           [tag (filter #(contains? (:account/user-tags %) tag) accounts)]))
                    (into {}))
        tag-set (set tags)]
    (assoc tagged
           :untagged (->> accounts
                          (remove #(seq (intersection (:account/user-tags %) tag-set)))))))

(defn- any-account-has-any-tag?
  [tags accounts]
  (boolean
    (when (seq tags)
      (some #(seq (intersection (set tags)
                                (:account/user-tags %)))
            accounts))))

(defn- process-budget-group
  [[account-type accounts] {:keys [period-count tags budget] :as options}]
  (if (any-account-has-any-tag? (set tags) accounts)
    (let [accounts-by-tag (group-by-tags tags accounts)]
      (->> (conj tags :untagged)
           (map (comp #(with-meta % {:account-type account-type})
                      #(process-tag-group % options)
                      #(vector % (accounts-by-tag %))))
           (remove zero-budget-report-item?))) ; TODO: Is there a better place for this?
    [(with-meta
       (budget-group-header period-count
                          account-type
                          (->> accounts
                               (map #(->budget-report-record % budget options))
                               (sort-by :report/difference)
                               (roll-up period-count)
                               (map #(dissoc % :account/parent))
                               (remove zero-budget-report-item?)))
       {:account-type account-type})]))

(defn- section-summary
  [caption period-count records]
  (let [[budget actual difference] (map #(sum % records)
                                        [:report/budget
                                         :report/actual
                                         :report/difference])]
    (with-precision 10
      #:report{:caption caption
               :style :summary
               :budget budget
               :actual actual
               :difference difference
               :percent-difference (when-not (zero? budget)
                                     (/ difference budget))
               :actual-per-period (/ actual period-count)})))

(defn- append-budget-summary
  [{:keys [period-count]} records]
  (let [income (filter #(= :income (-> % meta :account-type)) records)
        expense (filter #(= :expense (-> % meta :account-type)) records)
        income-budget (sum :report/budget income)
        income-actual (sum :report/actual income)
        expense-budget (sum :report/budget expense)
        expense-actual (sum :report/actual expense)
        budget (- income-budget expense-budget)
        actual (- income-actual expense-actual)
        difference (- actual budget)]
    (with-precision 10
      (concat income
              [(section-summary "Total Income" period-count income)]
              expense
              [(section-summary "Total Expense" period-count expense)]
              [#:report{:caption "Net"
                       :style :summary
                       :budget budget
                       :actual actual
                       :difference difference
                       :percent-difference (when-not (zero? budget)
                                             (/ difference budget))
                       :actual-per-period (/ actual period-count)}]))))

(defn- end-of-last-month []
  (-> (t/local-date)
      (t/minus (t/months 1))
      dates/last-day-of-the-month))

(defn- default-budget-end-date
  [{:budget/keys [start-date end-date]}]
  (->> [(end-of-last-month)
        (dates/last-day-of-the-month (t/local-date))
        end-date]
       (filter #(t/before? start-date %))
       (apply earliest)))

(defn- append-entity
  [{:keys [budget] :as ctx}]
  (assoc ctx :entity (-> budget :budget/entity entities/find)))

(defn- append-period-count
  [{:keys [budget as-of] :as ctx}]
  (assoc ctx
         :period-count
         (if (t/after? (:budget/end-date budget)
                       as-of)
           (inc (:index (budgets/period-containing budget as-of)))
           (get-in budget [:budget/period 0]))))

(defn- calc-budget-records
  [{:keys [budget as-of entity] :as ctx}]
  (->> (entities/select #:account{:entity (:budget/entity budget)
                                :type [:in [:income :expense]]})
       (rpt/valuate-accounts {:entity entity
                              :since (:budget/start-date budget)
                              :as-of as-of})
       (nest {:types [:income :expense]})
       unnest
       (group-by :account/type)
       (mapcat #(process-budget-group % ctx))
       (append-budget-summary ctx)))

(defn report
  "Returns a budget report"
  ([bdg]
   (report bdg {}))
  ([budget opts]
   (let [as-of (or (:as-of opts)
                   (default-budget-end-date budget))]
     {:items (-> opts
                 (merge {:budget budget
                         :as-of as-of})
                 append-entity
                 append-period-count
                 calc-budget-records)
      :title (format "%s: %s to %s"
                     (:budget/name budget)
                     (t/format (t/formatter "MMMM") (:budget/start-date budget))
                     (t/format (t/formatter "MMMM") as-of))})))
