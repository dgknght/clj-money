(ns clj-money.web.pages
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]))

(defn home
  "Renders the home page"
  []
  (html5
    [:html
     [:head
      [:title "Money"]]
     [:body
      [:h1 "Money"]]]))
