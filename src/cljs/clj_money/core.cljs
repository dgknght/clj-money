(ns clj-money.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [clj-money.state :as state]
            [clj-money.views.entities]
            [clj-money.views.commodities]
            [clj-money.api.entities :as entities]
            [clj-money.dom :refer [app-element]]
            [clj-money.layout :refer [with-layout]]))

(defn home-page []
  (with-layout
    [:h1 "This Is ClojureScript"]))

(secretary/defroute "/" []
  (r/render [home-page] (app-element)))

(defonce mounted? (atom false))

(defn mount-root []
  (when-not @mounted?
    (reset! mounted? true)
    (r/render home-page (app-element))))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler #(secretary/dispatch! %)
     :path-exists? #(secretary/locate-route %)})
  (accountant/dispatch-current!)
  (mount-root)
  (entities/get-all (fn [entities]
                      (reset! state/entities entities)
                      (when (seq entities)
                        (reset! state/current-entity (first entities))))))

(init!)
