(ns clj-money.views.commodities
  (:require [cljs.core.async :refer [chan >! go]]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [reagent.format :refer [currency-format]]
            [secretary.core :as secretary :include-macros true]
            [cljs-time.core :as t]
            [clj-money.components :refer [load-in-chunks
                                          load-on-scroll]]
            [clj-money.bootstrap :as bs]
            [clj-money.api.commodities :as commodities]
            [clj-money.api.prices :as prices]
            [clj-money.state :refer [app-state] :as state]
            [clj-money.notifications :as notify]
            [clj-money.plain-forms :as forms]
            [clj-money.util :as util]
            [clj-money.x-platform.util :refer [format-date]]))

(defn- load-commodities
  [page-state]
  (commodities/get-all (:id @state/current-entity)
                       #(swap! page-state assoc
                               :commodities (sort-by :name %)
                               :total (count %))
                       (notify/danger-fn "Unable to get a list of commodities: %s")))

(defn- save-commodity
  [page-state]
  (let [commodity (get-in @page-state [:selected])]
    (commodities/save (cond-> commodity
                        (not= "stock" (:type commodity)) (dissoc commodity :exchange))
                      (fn []
                        (swap! page-state dissoc :selected)
                        (load-commodities page-state))
                      (notify/danger-fn "Unable to save the commodity: %s"))))

(defn- commodity-form
  [page-state]
  (let [commodity (r/cursor page-state [:selected])
        types (make-reaction #(if (and (:id @commodity)
                                       (= (:id @commodity)
                                          (get-in @state/current-entity [:settings :default-commodity-id])))
                                ["currency"]
                                ["currency" "stock" "fund"]))]
    (fn []
      [:div#commodity-form.card
       [:div.card-header [:strong (str (if (:id @commodity) "Edit" "New")) " Commodity"]]
       [:div.card-body
        [forms/select-field commodity [:type] @types]
        (when (= "stock" (:type @commodity))
          [forms/select-field commodity [:exchange] ["nyse" "nasdaq"]])
        [forms/text-field commodity [:symbol] {:validate [:required]}]
        [forms/text-field commodity [:name] {:validate [:required]}]]
       [:div.card-footer
        [:button.btn.btn-primary {:title "Click here to save this commodity"
                                  :on-click #(save-commodity page-state)}
         (bs/icon-with-text :check "Save")]
        (util/space)
        [:button.btn.btn-danger {:title "Click here to discontinue this edit operation."
                                 :on-click #(swap! page-state dissoc :selected)}
         (bs/icon-with-text :x "Cancel")]]])))

(defn- delete
  [commodity page-state]
  (when (js/confirm (str "Are you sure you want to delete the commodity \"" (:name commodity) "\"?"))
    (commodities/delete commodity
                        #(load-commodities page-state)
                        notify/danger)))

(defn- truncate
  [value]
  (if (> 20 (count value))
    value
    [:span {:title value}
     (str
       (apply str (take 17 value))
       "...")]))

(defn- commodity-row
  [commodity page-state]
  (let  [default? (= (:id commodity)
                     (get-in @state/current-entity [:settings :default-commodity-id]))]
    ^{:key (:id commodity)}
    [:tr
     [:td (truncate (:name commodity))]
     [:td (:symbol commodity)]
     [:td (:exchange commodity)]
     [:td "coming soon..."]
     [:td
      [:div.btn-group
       [:button.btn.btn-info.btn-sm {:title "Click here to edit this commodity."
                                     :on-click (fn []
                                                 (swap! page-state #(-> %
                                                                        (dissoc :prices-commodity)
                                                                        (assoc :selected commodity)))
                                                 (util/set-focus "type"))}
        (bs/icon :pencil)]
       [:button.btn.btn-info.btn-sm {:title "Click here to view prices for this commodity."
                                     :disabled default?
                                     :on-click (fn []
                                                 (swap! page-state dissoc
                                                        :selected
                                                        :prices
                                                        :prices-commodity
                                                        :more-prices)
                                                 (js/setTimeout #(swap! page-state assoc
                                                                        :prices-commodity commodity
                                                                        :more-prices? true)
                                                                100))}
        (bs/icon :collection)]
       [:button.btn.btn-danger.btn-sm {:title "Click here to delete this commodity."
                                       :disabled default?
                                       :on-click #(delete commodity page-state)}
        (bs/icon :x-circle)]]]]))

(defn- commodity-list
  [page-state]
  (let [commodities (r/cursor page-state [:commodities])
        page-size (r/cursor page-state [:page-size])
        page-index (r/cursor page-state [:page-index])]
    (fn []
      [:table.table.table-striped.table-hover
       [:thead
        [:tr
         [:th "Name"]
         [:th "Symbol"]
         [:th "Exchange"]
         [:th "Latest Price"]
         [:th (util/space)]]]
       [:tbody
        (->> @commodities
             (drop (* @page-size @page-index))
             (take @page-size)
             (map #(commodity-row % page-state))
             doall)]])))

(defn- delete-price
  [price page-state]
  (when (js/confirm (str "Are you sure you want to delete the price from " (format-date (:trade-date price)) "?"))
    (prices/delete price
                   (fn []
                     (swap! page-state
                            update-in
                            [:prices]
                            (fn [prices]
                              (remove #(= (:id price) (:id %)) prices))))
                   (notify/danger-fn "Unable to delete the price: %s"))))

(defn- price-row
  [price page-state]
  ^{:key (str "price-row-" (:id price))}
  [:tr
   [:td.text-right (format-date (:trade-date price))]
   [:td.text-right (currency-format (:price price))]
   [:td
    [:div.btn-group
     [:button.btn.btn-info.btn-sm {:title "Click here to edit this price."
                                   :on-click (fn []
                                               (swap! page-state assoc :selected-price price)
                                               (util/set-focus "trade-date"))}
      (bs/icon :pencil)]
     [:button.btn.btn-danger.btn-sm {:title "Click here to remove this price."
                                     :on-click #(delete-price price page-state)}
      (bs/icon :x-circle)]]]])

(defn- init-price-loading
  [page-state]
  (let [end (t/local-date (inc (t/year (t/today))) 1 1)]
    (load-in-chunks
      {:start (t/minus- end (t/years 10))
       :end end
       :interval (t/years 1)
       :ctl-chan (:prices-ctl-chan @page-state)
       :fetch-fn (fn [date-range callback-fn]
                   (prices/search {:trade-date date-range
                                   :commodity-id (get-in @page-state [:prices-commodity :id])}
                                  callback-fn
                                  (notify/danger-fn "Unable to fetch prices: %s")))
       :receive-fn #(swap! page-state update-in [:prices] (fnil into []) %)
       :finish-fn #(swap! page-state assoc :more-prices? false)})))

(defn- price-list
  [page-state]
  (let [prices (r/cursor page-state [:prices])
        commodity (r/cursor page-state [:prices-commodity])
        more-prices? (r/cursor page-state [:more-prices?])
        ctl-chan (r/cursor page-state [:prices-ctl-chan])]
    (init-price-loading page-state)
    (fn []
      [:div.card
       [:div.card-header [:strong (str (:name @commodity) " Prices")]]
       [:div#prices-container {:style {:max-height "40em" :overflow "auto"}}
        [:table.table.table-striped.table-hover
         [:thead
          [:tr
           [:th.text-right "Trade Date"]
           [:th.text-right "Price"]
           [:th (util/space)]] ]
         [:tbody
          (if @prices
            (doall (map #(price-row % page-state) @prices))
            [:tr [:td {:col-span 3} [:span.inline-status "Loading..."]]])]]]
       [:div.card-footer.d-flex
        [:button.btn.btn-primary {:title "Click here to add a new price for this commodity."
                                  :on-click (fn []
                                              (swap! page-state
                                                     assoc
                                                     :selected-price
                                                     {:commodity-id (:id @commodity)})
                                              (util/set-focus "trade-date"))}
         (bs/icon-with-text :plus "Add")]
        (util/space)
        [:button.btn.btn-light {:on-click #(swap! page-state dissoc :prices-commodity)}
         (bs/icon-with-text :x "Cancel")]
        [:span.ml-auto
         [load-on-scroll {:target "prices-container"
                          :can-load-more? (fn [] @more-prices?)
                          :load-fn #(go (>! @ctl-chan :fetch))}]]]])))

(defn- comp-prices
  [{d1 :trade-date} {d2 :trade-date}]
  (cond
    (= d1 d2) 0
    (t/before? d1 d2) -1
    :else 1))

(defn- comp-prices-rev
  [p1 p2]
  (comp-prices p2 p1))

(defn- save-price
  [page-state]
  (let [price (get-in @page-state [:selected-price])]
    (prices/save price
                 (fn [result]
                   (let [update-fn (if (:id price)
                                     (fn [prices]
                                       (mapv #(if (= (:id %) (:id price))
                                                result
                                                %)
                                             prices))
                                     (fn [prices]
                                       (->> (conj prices result)
                                            (sort comp-prices-rev)
                                            vec)))]
                     (swap! page-state #(-> %
                                            (dissoc :selected-price)
                                            (update-in [:prices] update-fn)))))
                 (notify/danger-fn "Unable to save the price: %s"))))

(defn- price-form
  [page-state]
  (let [price (r/cursor page-state [:selected-price])]
    (fn []
      [:div.card.mt-2
       [:div.card-header [:strong (str (if  (:id @price) "Edit" "New") " Price")]]
       [:div.card-body
        [forms/date-field price [:trade-date]]
        [forms/float-field price [:price]]]
       [:div.card-footer
        [:button.btn.btn-primary {:title "Click here to save this price."
                                  :on-click #(save-price page-state)}
         (bs/icon-with-text :check "Save")]
        (util/space)
        [:button.btn.btn-light {:title "Click here to cancel this update."
                                :on-click #(swap! page-state dissoc :selected-price)}
         (bs/icon-with-text :x "Cancel")]]])))

(defn- index []
  (let [page-state (r/atom {:page-size 16
                            :page-index 0
                            :prices-ctl-chan (chan)})
        selected (r/cursor page-state [:selected])
        commodities (r/cursor page-state [:commodities])
        total (r/cursor page-state [:total])
        page-size (r/cursor page-state [:page-size])
        prices-commodity (r/cursor page-state [:prices-commodity])
        selected-price (r/cursor page-state [:selected-price])]
    (load-commodities page-state)
    (add-watch app-state :current-entity (fn [field _sender _before after]
                                           (when (get-in after [field])
                                             (load-commodities page-state))))
    (fn []
      [:div.mt-5
       [:h1 "Commodities"]
       [:div.row
        [:div.col
         [commodity-list page-state]
         (when (and (seq @commodities)
                    (> @total @page-size))
           [bs/pagination page-state])
         [:button.btn.btn-primary
          {:title "Click here to add a new commodity."
           :on-click (fn []
                       (swap! page-state
                              assoc
                              :selected
                              {:entity-id (:id @state/current-entity)
                               :type "stock"
                               :exchange "nyse"})
                       (util/set-focus "type"))
           :disabled (boolean @selected)}
          (bs/icon-with-text :plus "Add")]]
        (when @prices-commodity
          [:div.col
           [price-list page-state]
           (when @selected-price
             [price-form page-state])])
        (when @selected
          [:div.col-md-4
           [commodity-form page-state]])]])))

(secretary/defroute "/commodities" []
  (swap! app-state assoc :page #'index))
