(ns clj-money.chart
  (:require [reagent.core :as r]
            ["chartjs" :as chartjs]))

(defn chart
  "Renders a Chart.js chart from the given configuration, which is a map
  with the same structure as the configuration object Chart.js accepts."
  [_config & _attrs]
  (let [canvas (atom nil)
        instance (atom nil)]
    (r/create-class
      {:display-name "chart"
       :component-did-mount
       (fn [this]
         (reset! instance
                 (chartjs/create @canvas
                                 (clj->js (second (r/argv this))))))
       :component-did-update
       (fn [this _]
         (chartjs/update @instance
                         (clj->js (second (r/argv this)))))
       :component-will-unmount
       (fn [_]
         (chartjs/destroy @instance))
       :reagent-render
       (fn [_config & [attrs]]
         [:div attrs
          [:canvas {:ref #(reset! canvas %)}]])})))
