(ns clj-money.web.apps
  (:refer-clojure :exclude [update])
  (:require [ring.util.response :refer [redirect]]
            [hiccup.page :refer [html5 include-js]]
            [clj-money.web.shared :refer [head]]))

(defn index
  [_]
  (redirect "/apps/default"))

(defn show
  [_]
  (html5
    [:html {:lang "en"}
     (head "clj-money" {})
     [:body
      [:div#app]
      (include-js "/js/app/main.js")]]))
