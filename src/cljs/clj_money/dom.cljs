(ns clj-money.dom)

(defn app-element
  "Returns the DOM element into which the application is rendered"
  []
  (.getElementById js/document "app"))
