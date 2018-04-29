(ns clj-money.web.apps
  (:refer-clojure :exclude [update])
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [clj-money.web.shared :refer [head]]))

(defn index
  [_]
  (redirect "/apps/default"))

(defn show
  [{params :params}]
  (html5
    [:html {:lang "en"}
     (head "clj-money" {})
     [:body
      [:div#app]
      (include-js "/js/app.js")]]))
