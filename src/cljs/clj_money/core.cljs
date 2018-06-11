(ns clj-money.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [clj-money.util :refer [path]]
            [clj-money.state :as state]
            [clj-money.data :as data]
            [clj-money.notifications :refer [notifications unnotify]]
            [clj-money.entities :as entities]
            [clj-money.commodities :as commodities]
            [clj-money.bootstrap :as bootstrap]
            [clj-money.dom :refer [app-element]]
            [clj-money.layout :refer [with-layout]]))

(defn home-page []
  (.log js/console "home-page")
  (with-layout
    [:h1 "This Is ClojureScript"]))

(secretary/defroute "/" []
  (r/render home-page (app-element)))

(defn mount-root []
  (r/render home-page (app-element)))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler #(secretary/dispatch! %)
     :path-exists? #(secretary/locate-route %)})
  (accountant/dispatch-current!)
  (mount-root)
  (data/get-entities (fn [entities]
                       (reset! state/entities entities)
                       (when (seq entities)
                         (reset! state/current-entity (first entities))))))

(init!)
