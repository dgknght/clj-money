(ns clj-money.views.calendar
  (:require [cljs-time.core :as t]
            [reagent.core :as r]
            [clj-money.util :refer [serialize-date]]
            [clj-money.calendar :as cal]))

(defn- day-cell
  [{:keys [date
           in-month?
           today?
           selected?]}
   {:keys [day-fn small? on-day-click]
    :or {on-day-click identity}}]

  ^{:key (str "day-" (serialize-date date))}
  [:td.calendar-cell {:class [(when small? "text-center")
                              (when-not in-month? "out-of-month")
                              (when today? "calendar-today")
                              (when selected? "calendar-selected")]
                      :on-click #(on-day-click date)}
   [:span.calendar-day-header
    {:style {:cursor :pointer}}
    (t/day date)]
   (when day-fn
     [:span.calendar-day-content
      (day-fn date)])])

(defn- week
  [days opts]
  ^{:key (str "week-" (serialize-date (:date (first days))))}
  [:tr
   (doall (map #(day-cell % opts) days))])

(defn calendar
  [ctl-state {:keys [small? on-next on-previous]
              :or {on-next identity
                   on-previous identity}
              :as opts}]
  (let [c (r/cursor ctl-state [:calendar])]
    (fn []
      [:table.table.borderless-table {:class (when small? "table-sm")}
       [:thead
        [:tr
         [:th.text-center {:col-span 2}
          [:button.btn.btn-sm.btn-light
           {:on-click (fn [e]
                        (.preventDefault e)
                        (swap! ctl-state update-in [:calendar] cal/prev-month)
                        (on-previous))}
           "<<"]]
         [:th.text-center {:col-span 3} (:title @c)]
         [:th.text-center {:col-span 2}
          [:button.btn.btn-sm.btn-light
           {:on-click (fn [e]
                        (.preventDefault e)
                        (swap! ctl-state update-in [:calendar] cal/next-month)
                        (on-next))}
           ">>"]]]
        [:tr
         (->> (:week-days @c)
              (map (comp #(with-meta (vector :th.text-center {:scope "col"
                                                              :width "14%"}
                                             %)
                            {:key (str "day-of-week-" %)})
                         :abbreviation))
              doall)]]
       [:tbody
        (doall (map #(week % opts) (:weeks @c)))]])))
