(ns clj-money.views.users
  (:require [secretary.core :as secretary :include-macros true]
            [clj-money.state :refer [app-state]]
            [clj-money.util :refer [google-g]]))

(defn- login []
  [:div.mt-5
   [:h1 "Login"]
   [:a#login.btn.btn-light {:href "/auth/google/start"
                              :title "Click here to sign in with a Google account"}
    (google-g)
    [:span "Sign in with Google"]]])

(secretary/defroute "/login" []
  (swap! app-state assoc :page #'login))
