(ns clj-money.views.transactions.page-test
  (:require [cljs.test :refer [deftest is]]
            [secretary.core :as secretary]
            [clj-money.state :refer [app-state]]
            [clj-money.views.transactions.page :as trx-page]))

(deftest get-the-path-for-a-transaction
  (is (= "/transactions/101"
         (trx-page/path {:id 101}))
      "The path identifies the transaction")
  (is (= "/transactions/101?account-id=201"
         (trx-page/path {:id 101} :account-id 201))
      "The account to show it from is included when given"))

(defn- dispatch
  [path]
  (let [page (:page @app-state)]
    (secretary/dispatch! path)
    (let [[_ id query-params] ((:page @app-state))]
      (swap! app-state assoc :page page)
      [id query-params])))

(deftest the-path-is-routed-to-the-page
  (doseq [id [101 "101:transaction" "3f2504e0-4f89-11d3-9a0c-0305e82c3301"]]
    (is (= [(str id) {:account-id "201:account"}]
           (dispatch (trx-page/path {:id id} :account-id "201:account")))
        (str "The page gets the transaction id " id " and the account id"))
    (is (= [(str id) nil]
           (dispatch (trx-page/path {:id id})))
        (str "The page gets the transaction id " id " without an account"))))
