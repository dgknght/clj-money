(ns clj-money.db.sql.partitioning-test
  (:require [clojure.test :refer [deftest testing is]]
            [next.jdbc :as jdbc]
            [java-time.api :as t]
            [clj-money.db.sql.partitioning :as prt]))

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
    (let [expected #{["create table if not exists price_y2001 partition of price for values from ('2001-01-01') to ('2002-01-01');"]
                     ["create table if not exists transaction_y2001 partition of transaction for values from ('2001-01-01') to ('2002-01-01');"]
                     ["create table if not exists transaction_item_y2001 partition of transaction_item for values from ('2001-01-01') to ('2002-01-01');"]
                     ["create table if not exists reconciliation_y2001_y2005 partition of reconciliation for values from ('2001-01-01') to ('2006-01-01');"]
                     ["create table if not exists cached_price_y2001 partition of cached_price for values from ('2001-01-01') to ('2002-01-01');"]}
          cmds (with-cmd-intercept
                 (prt/create-partition-tables
                  (t/local-date 2001 1 1)
                  (t/local-date 2001 12 31)
                  {:silent true
                   :intervals {:default {:interval-type :year
                                         :interval-count 1}}}))]
      (is (= expected cmds))))
  (testing "start on a non-anchor year"
    (let [expected #{["create table if not exists price_y2002 partition of price for values from ('2002-01-01') to ('2003-01-01');"]
                     ["create table if not exists transaction_y2002 partition of transaction for values from ('2002-01-01') to ('2003-01-01');"]
                     ["create table if not exists transaction_item_y2002 partition of transaction_item for values from ('2002-01-01') to ('2003-01-01');"]
                     ["create table if not exists reconciliation_y2001_y2005 partition of reconciliation for values from ('2001-01-01') to ('2006-01-01');"]
                     ["create table if not exists cached_price_y2002 partition of cached_price for values from ('2002-01-01') to ('2003-01-01');"]}
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
    (let [expected #{["create table if not exists price_y2020_m01 partition of price for values from ('2020-01-01') to ('2020-02-01');"]
                     ["create table if not exists price_y2020_m02 partition of price for values from ('2020-02-01') to ('2020-03-01');"]
                     ["create table if not exists transaction_y2020_m01 partition of transaction for values from ('2020-01-01') to ('2020-02-01');"]
                     ["create table if not exists transaction_y2020_m02 partition of transaction for values from ('2020-02-01') to ('2020-03-01');"]
                     ["create table if not exists transaction_item_y2020_m01 partition of transaction_item for values from ('2020-01-01') to ('2020-02-01');"]
                     ["create table if not exists transaction_item_y2020_m02 partition of transaction_item for values from ('2020-02-01') to ('2020-03-01');"]
                     ["create table if not exists reconciliation_y2020_m01 partition of reconciliation for values from ('2020-01-01') to ('2020-02-01');"]
                     ["create table if not exists reconciliation_y2020_m02 partition of reconciliation for values from ('2020-02-01') to ('2020-03-01');"]
                     ["create table if not exists cached_price_y2020_m01 partition of cached_price for values from ('2020-01-01') to ('2020-02-01');"]
                     ["create table if not exists cached_price_y2020_m02 partition of cached_price for values from ('2020-02-01') to ('2020-03-01');" ]}
          cmds (with-cmd-intercept
                 (prt/create-partition-tables
                   (t/local-date 2020 1 1)
                   (t/local-date 2020 2 29)
                   {:silent true
                    :rules {:price            {:interval-type :month
                                               :interval-count 1}
                            :cached_price     {:interval-type :month
                                               :interval-count 1}
                            :transaction      {:interval-type :month
                                               :interval-count 1}
                            :transaction_item {:interval-type :month
                                               :interval-count 1}
                            :reconciliation   {:interval-type :month
                                               :interval-count 1}}}))]
      (is (= expected cmds))))
  (testing "start on a non-anchor month"
    (let [expected #{["create table if not exists price_y2020_m01_m02 partition of price for values from ('2020-01-01') to ('2020-03-01');"]
                     ["create table if not exists price_y2020_m03_m04 partition of price for values from ('2020-03-01') to ('2020-05-01');"]
                     ["create table if not exists transaction_y2020_m01_m02 partition of transaction for values from ('2020-01-01') to ('2020-03-01');"]
                     ["create table if not exists transaction_y2020_m03_m04 partition of transaction for values from ('2020-03-01') to ('2020-05-01');"]
                     ["create table if not exists transaction_item_y2020_m01_m02 partition of transaction_item for values from ('2020-01-01') to ('2020-03-01');"]
                     ["create table if not exists transaction_item_y2020_m03_m04 partition of transaction_item for values from ('2020-03-01') to ('2020-05-01');"]
                     ["create table if not exists reconciliation_y2020_m01_m02 partition of reconciliation for values from ('2020-01-01') to ('2020-03-01');"]
                     ["create table if not exists reconciliation_y2020_m03_m04 partition of reconciliation for values from ('2020-03-01') to ('2020-05-01');"]
                     ["create table if not exists cached_price_y2020_m01_m02 partition of cached_price for values from ('2020-01-01') to ('2020-03-01');"]
                     ["create table if not exists cached_price_y2020_m03_m04 partition of cached_price for values from ('2020-03-01') to ('2020-05-01');"]}
          cmds (with-cmd-intercept
                 (prt/create-partition-tables
                   (t/local-date 2020 2 1)
                   (t/local-date 2020 3 31)
                   {:silent true
                    :rules {:price            {:interval-type :month
                                               :interval-count 2}
                            :cached_price     {:interval-type :month
                                               :interval-count 2}
                            :transaction      {:interval-type :month
                                               :interval-count 2}
                            :transaction_item {:interval-type :month
                                               :interval-count 2}
                            :reconciliation   {:interval-type :month
                                               :interval-count 2}}}))]
      (is (= expected cmds)))))
