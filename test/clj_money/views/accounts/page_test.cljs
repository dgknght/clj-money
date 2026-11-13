(ns clj-money.views.accounts.page-test
  (:require [cljs.test :refer [deftest is]]
            [secretary.core :as secretary]
            [clj-money.state :refer [app-state]]
            [clj-money.views.accounts.page :as accts-page]))

(deftest get-the-path-to-reconcile-an-account
  (is (= "/accounts/101/reconcile"
         (accts-page/reconcile-path {:id 101}))
      "The path identifies the account"))

(defn- dispatch
  [path]
  (let [page (:page @app-state)]
    (secretary/dispatch! path)
    (let [[_ id action] ((:page @app-state))]
      (swap! app-state assoc :page page)
      [id action])))

(deftest the-reconcile-path-is-routed-to-the-page
  (doseq [id [101 "101:account" "3f2504e0-4f89-11d3-9a0c-0305e82c3301"]]
    (is (= [(str id) :reconcile]
           (dispatch (accts-page/reconcile-path {:id id})))
        (str "The page gets the account id " id))))

(deftest the-account-paths-are-routed-to-the-page
  (is (= ["101:account" :view]
         (dispatch "/accounts/101%3Aaccount"))
      "The account path views the account")
  (is (= ["101:account" :edit]
         (dispatch "/accounts/101%3Aaccount/edit"))
      "The edit path edits the account"))
