(ns clj-money.web.accounts
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all])
  (:use [clj-money.web.shared :refer :all]))

(defn index
  "Renders the list of accounts"
  []
  (layout
    "Accounts" {}
    "Account list will go here."))
