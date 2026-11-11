(ns clj-money.views.trades.form-test
  (:require [cljs.test :refer [deftest testing is use-fixtures]]
            [cljs-time.core :as t]
            [clj-money.state :as state]
            [clj-money.views.trades.form :as trades]))

(def ^:private entity {:id 101})

(use-fixtures :each
  {:before #(reset! state/current-entity entity)
   :after #(reset! state/current-entity nil)})

(def ^:private ira
  {:id 1
   :account/name "IRA"
   :account/system-tags #{:trading}})

(def ^:private aapl
  {:id 2
   :account/name "AAPL"
   :account/parent ira
   :account/commodity {:id 201}
   :account/system-tags #{:tradable}})

(deftest start-a-trade
  (testing "from a trading account"
    (is (= #:trade{:entity entity
                   :action :buy
                   :date (t/today)
                   :commodity-account nil
                   :commodity nil
                   :account ira}
           (trades/new-trade ira))
        "The account is the one the shares are bought with, and the commodity is chosen"))
  (testing "from a tradable account"
    (is (= #:trade{:entity entity
                   :action :buy
                   :date (t/today)
                   :commodity-account aapl
                   :commodity 201
                   :account ira}
           (trades/new-trade aapl))
        "The shares of the account's commodity are bought with its parent")))

(deftest start-a-dividend
  (is (= #:trade{:entity entity
                 :dividend? true
                 :action :buy
                 :date (t/today)
                 :account ira}
         (trades/new-dividend ira))))
