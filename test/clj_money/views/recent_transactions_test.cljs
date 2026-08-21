(ns clj-money.views.recent-transactions-test
  (:require [cljs.test :refer [deftest is testing]]
            [clj-money.dates :as dates]
            [clj-money.views.recent-transactions :as recent-trx]))

(def ^:private transactions
  [{:id 1
    :transaction/description "Older, entered later"
    :transaction/transaction-date (dates/local-date "2020-01-01")
    :transaction/created-at (dates/local-date-time "2020-01-05T09:00:00")}
   {:id 2
    :transaction/description "Newer, entered first"
    :transaction/transaction-date (dates/local-date "2020-01-10")
    :transaction/created-at (dates/local-date-time "2020-01-02T09:00:00")}
   {:id 3
    :transaction/description "Middle"
    :transaction/transaction-date (dates/local-date "2020-01-05")
    :transaction/created-at (dates/local-date-time "2020-01-03T09:00:00")}])

(deftest sort-by-created-at-descending-by-default
  (is (= ["Older, entered later" "Middle" "Newer, entered first"]
         (->> (recent-trx/sort-and-limit transactions recent-trx/default-settings)
              (map :transaction/description)))))

(deftest sort-by-created-at-ascending
  (is (= ["Newer, entered first" "Middle" "Older, entered later"]
         (->> (recent-trx/sort-and-limit transactions (assoc recent-trx/default-settings :dir :asc))
              (map :transaction/description)))))

(deftest sort-by-transaction-date-descending
  (is (= ["Newer, entered first" "Middle" "Older, entered later"]
         (->> (recent-trx/sort-and-limit transactions (assoc recent-trx/default-settings :sort-on :transaction/transaction-date))
              (map :transaction/description)))))

(deftest limit-the-number-of-results
  (is (= ["Older, entered later"]
         (->> (recent-trx/sort-and-limit transactions (assoc recent-trx/default-settings :limit 1))
              (map :transaction/description)))))

(deftest break-ties-on-id
  (testing "descending sort favors the higher (more recently inserted) id"
    (is (= [2 1]
           (->> (recent-trx/sort-and-limit
                  [{:id 1 :transaction/created-at (dates/local-date-time "2020-01-01T09:00:00")}
                   {:id 2 :transaction/created-at (dates/local-date-time "2020-01-01T09:00:00")}]
                  recent-trx/default-settings)
                (map :id)))))
  (testing "ascending sort favors the lower (earlier inserted) id"
    (is (= [1 2]
           (->> (recent-trx/sort-and-limit
                  [{:id 1 :transaction/created-at (dates/local-date-time "2020-01-01T09:00:00")}
                   {:id 2 :transaction/created-at (dates/local-date-time "2020-01-01T09:00:00")}]
                  (assoc recent-trx/default-settings :dir :asc))
                (map :id))))))
