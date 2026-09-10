(ns clj-money.api.trading-test
  (:require [cljs.test :refer [deftest is]]
            [clj-money.decimal :as d]
            [clj-money.api :as api]
            [clj-money.api.trading :as trading]))

(def ^:private trade
  #:trade{:entity {:id 1}
          :account {:id 2}
          :commodity {:id 3}
          :date "2026-09-10"
          :action :sell
          :shares (d/d "10")
          :value (d/d "1234.56")
          :fee (d/d "5.25")
          :value-includes-fee? true})

(deftest a-sell-with-a-fee-included-in-the-value-normalizes-correctly
  (let [posted-value (atom nil)]
    (with-redefs [api/post (fn [_url payload _opts]
                              (reset! posted-value (:trade/value payload)))]
      (trading/create trade))
    (is (= (d/d "1239.81") @posted-value)
        "The fee is added back to the value for a sell, using decimal arithmetic instead of string concatenation")))

(deftest a-buy-with-a-fee-included-in-the-value-normalizes-correctly
  (let [posted-value (atom nil)]
    (with-redefs [api/post (fn [_url payload _opts]
                              (reset! posted-value (:trade/value payload)))]
      (trading/create (assoc trade :trade/action :buy)))
    (is (= (d/d "1229.31") @posted-value)
        "The fee is subtracted from the value for a buy")))
