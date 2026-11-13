(ns clj-money.commands-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [clj-money.commands :as cmds]))

(def ^:private nav-items
  [{:id :accounts :infered-label "Accounts" :path "/accounts"}
   {:id :scheduled :infered-label "Scheduled" :path "/scheduled"}
   {:id :reports :infered-label "Reports" :path "/reports"}])

(def ^:private accounts
  [{:id 101 :account/path ["Checking"]}
   {:id 102 :account/path ["Credit Card"]}
   {:id 103 :account/path ["Groceries"] :account/hidden true}
   {:id 104 :account/path ["Savings" "Car Fund"]}])

(def ^:private commands
  (concat (cmds/nav-commands nav-items)
          (cmds/account-commands accounts)))

(defn- labels
  [input]
  (map :label (cmds/search commands input)))

(deftest blank-input-offers-nothing
  (is (empty? (cmds/search commands nil)))
  (is (empty? (cmds/search commands "  "))))

(deftest go-to-a-page
  (is (= {:label "Go to Accounts"
          :path "/accounts"}
         (select-keys (first (cmds/search commands "acc"))
                      [:label :path]))
      "The page whose name starts with the input comes first"))

(deftest act-on-an-account
  (is (= [["View transactions for Checking" "/accounts/101"]
          ["Edit account Checking" "/accounts/101/edit"]]
         (->> (cmds/search commands "check")
              (take 2)
              (map (juxt :label :path))))
      "An account's name offers its transactions and editing it")
  (is (= "View transactions for Savings/Car Fund"
         (first (labels "car fund")))
      "A child account is matched by its own name")
  (is (= "View transactions for Savings/Car Fund"
         (first (labels "savings/car")))
      "A child account is matched by its full path")
  (is (not-any? #(re-find #"Groceries" %)
                (labels "groc"))
      "Hidden accounts are left out"))

(deftest rank-the-matches
  (testing "a prefix beats a later word, which beats a substring"
    (is (= ["Name things" "Ignore name" "Rename"]
           (->> (cmds/search [{:label "Rename"}
                              {:label "Ignore name"}
                              {:label "Name things"}
                              {:label "Other"}]
                             "name")
                butlast
                (map :label)))))
  (testing "a keyword ranks the command as well as the label"
    (is (= ["Go to Accounts" "Plain account"]
           (->> (cmds/search [{:label "Plain account"}
                              {:label "Go to Accounts"
                               :keywords ["Accounts"]}]
                             "acc")
                butlast
                (map :label))))))

(deftest fall-back-to-a-transaction-search
  (is (= {:label "Search transactions for 'Kroger & co'"
          :path "/search?date-range=this-year&description=Kroger%20%26%20co"}
         (select-keys (last (cmds/search commands " Kroger & co "))
                      [:label :path]))
      "The last command searches the transactions for the input")
  (is (= ["Search transactions for 'zzz'"]
         (labels "zzz"))
      "Input that matches nothing only offers the search"))
