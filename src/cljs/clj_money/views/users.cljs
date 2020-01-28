(ns clj-money.views.users
  (:require [secretary.core :as secretary :include-macros true]
            [clj-money.state :refer [app-state]]))

(defn- login []
  [:div
   [:h1 "Login"]
   [:a.btn.btn-primary {:href "/auth/google/start"
                        :title "Click here to sign in with a Google account"}
    "Google"]])

(secretary/defroute "/login" []
  (swap! app-state assoc :page #'login))
