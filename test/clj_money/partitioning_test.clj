(ns clj-money.partitioning-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.jdbc :as jdbc]
            [java-time.api :as t]
            [clj-money.partitioning :as prt]))

(defmacro ^:private with-cmd-intercept
  [& body]
  `(let [cmds# (atom #{})]
     (with-redefs [jdbc/execute! (fn [_conn# cmd#]
                                   (swap! cmds# conj cmd#))]
       ~@body)
     @cmds#))

(deftest partition-by-year
  ; NB: The lower bound of the range is inclusive and the upper bound is exclusive
  (testing "start on an anchor year"
    (let [expected #{"create table if not exists prices_y2001 partition of prices for values from ('2001-01-01') to ('2002-01-01');"
                     "create table if not exists transactions_y2001 partition of transactions for values from ('2001-01-01') to ('2002-01-01');"
                     "create table if not exists transaction_items_y2001 partition of transaction_items for values from ('2001-01-01') to ('2002-01-01');"
                     "create table if not exists reconciliations_y2001_y2005 partition of reconciliations for values from ('2001-01-01') to ('2006-01-01');"
                     "create table if not exists cached_prices_y2001 partition of cached_prices for values from ('2001-01-01') to ('2002-01-01');"}
          cmds (with-cmd-intercept
                 (prt/create-partition-tables
                  (t/local-date 2001 1 1)
                  (t/local-date 2001 12 31)
                  {:silent true
                   :intervals {:default {:interval-type :year
                                         :interval-count 1}}}))]
      (is (= expected cmds))))
  (testing "start on a non-anchor year"
    (let [expected #{"create table if not exists prices_y2002 partition of prices for values from ('2002-01-01') to ('2003-01-01');"
                     "create table if not exists transactions_y2002 partition of transactions for values from ('2002-01-01') to ('2003-01-01');"
                     "create table if not exists transaction_items_y2002 partition of transaction_items for values from ('2002-01-01') to ('2003-01-01');"
                     "create table if not exists reconciliations_y2001_y2005 partition of reconciliations for values from ('2001-01-01') to ('2006-01-01');"
                     "create table if not exists cached_prices_y2002 partition of cached_prices for values from ('2002-01-01') to ('2003-01-01');" }
          cmds (with-cmd-intercept
                 (prt/create-partition-tables
                  (t/local-date 2002 1 1)
                  (t/local-date 2002 12 31)
                  {:silent true
                   :intervals {:default {:interval-type :year
                                         :interval-count 1}}}))]
      (is (= expected cmds)))))

(deftest partition-by-month
  (testing "start on an anchor month"
    (let [expected #{"create table if not exists prices_y2020_m01 partition of prices for values from ('2020-01-01') to ('2020-02-01');"
                     "create table if not exists prices_y2020_m02 partition of prices for values from ('2020-02-01') to ('2020-03-01');"
                     "create table if not exists transactions_y2020_m01 partition of transactions for values from ('2020-01-01') to ('2020-02-01');"
                     "create table if not exists transactions_y2020_m02 partition of transactions for values from ('2020-02-01') to ('2020-03-01');"
                     "create table if not exists transaction_items_y2020_m01 partition of transaction_items for values from ('2020-01-01') to ('2020-02-01');"
                     "create table if not exists transaction_items_y2020_m02 partition of transaction_items for values from ('2020-02-01') to ('2020-03-01');"
                     "create table if not exists reconciliations_y2020_m01 partition of reconciliations for values from ('2020-01-01') to ('2020-02-01');"
                     "create table if not exists reconciliations_y2020_m02 partition of reconciliations for values from ('2020-02-01') to ('2020-03-01');"
                     "create table if not exists cached_prices_y2020_m01 partition of cached_prices for values from ('2020-01-01') to ('2020-02-01');"
                     "create table if not exists cached_prices_y2020_m02 partition of cached_prices for values from ('2020-02-01') to ('2020-03-01');" }
          cmds (with-cmd-intercept
                 (prt/create-partition-tables
                  (t/local-date 2020 1 1)
                  (t/local-date 2020 2 29)
                  {:silent true
                   :rules {:prices            {:interval-type :month
                                               :interval-count 1}
                           :cached_prices     {:interval-type :month
                                               :interval-count 1}
                           :transactions      {:interval-type :month
                                               :interval-count 1}
                           :transaction_items {:interval-type :month
                                               :interval-count 1}
                           :reconciliations   {:interval-type :month
                                               :interval-count 1}}}))]
      (is (= expected cmds))))
  (testing "start on a non-anchor month"
    (let [expected #{"create table if not exists prices_y2020_m01_m02 partition of prices for values from ('2020-01-01') to ('2020-03-01');"
                     "create table if not exists prices_y2020_m03_m04 partition of prices for values from ('2020-03-01') to ('2020-05-01');"
                     "create table if not exists transactions_y2020_m01_m02 partition of transactions for values from ('2020-01-01') to ('2020-03-01');"
                     "create table if not exists transactions_y2020_m03_m04 partition of transactions for values from ('2020-03-01') to ('2020-05-01');"
                     "create table if not exists transaction_items_y2020_m01_m02 partition of transaction_items for values from ('2020-01-01') to ('2020-03-01');"
                     "create table if not exists transaction_items_y2020_m03_m04 partition of transaction_items for values from ('2020-03-01') to ('2020-05-01');"
                     "create table if not exists reconciliations_y2020_m01_m02 partition of reconciliations for values from ('2020-01-01') to ('2020-03-01');"
                     "create table if not exists reconciliations_y2020_m03_m04 partition of reconciliations for values from ('2020-03-01') to ('2020-05-01');"
                     "create table if not exists cached_prices_y2020_m01_m02 partition of cached_prices for values from ('2020-01-01') to ('2020-03-01');"
                     "create table if not exists cached_prices_y2020_m03_m04 partition of cached_prices for values from ('2020-03-01') to ('2020-05-01');"}
          cmds (with-cmd-intercept
                 (prt/create-partition-tables
                  (t/local-date 2020 2 1)
                  (t/local-date 2020 3 31)
                  {:silent true
                   :rules {:prices            {:interval-type :month
                                               :interval-count 2}
                           :cached_prices     {:interval-type :month
                                               :interval-count 2}
                           :transactions      {:interval-type :month
                                               :interval-count 2}
                           :transaction_items {:interval-type :month
                                               :interval-count 2}
                           :reconciliations   {:interval-type :month
                                               :interval-count 2}}}))]
      (is (= expected cmds)))))
