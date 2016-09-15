(ns clj-money.web.accounts
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]))

(defn index
  "Renders the list of accounts"
  []
  (clj-money.web.pages/layout
    "Accounts" "Accounts"
    "Account list will go here."))
