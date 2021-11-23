(ns clj-money.views.commodities
  (:require [cljs.core.async :refer [chan <! >! go go-loop]]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [reagent.format :refer [currency-format]]
            [secretary.core :as secretary :include-macros true]
            [cljs-time.core :as t]
            [dgknght.app-lib.core :refer [fmin
                                          fmax]]
            [dgknght.app-lib.web :refer [format-date]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.notifications :as notify]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [dgknght.app-lib.busy :refer [busy +busy -busy]]
            [clj-money.dates :as dates]
            [clj-money.views.util :refer [handle-error]]
            [clj-money.components :refer [load-in-chunks
                                          load-on-scroll]]
            [clj-money.api.commodities :as commodities]
            [clj-money.api.prices :as prices]
            [clj-money.state :refer [app-state
                                     current-entity]]
            [dgknght.app-lib.forms :as forms]))

(defn- load-commodities
  [page-state]
  (+busy page-state)
  (commodities/select (fn [result]
                        (swap! page-state #(-> %
                                               -busy
                                               (assoc :commodities (sort-by :name result)))))
                      (handle-error page-state "Unable to get a list of commodities: %s")))

(defn- save-commodity
  [page-state]
  (let [commodity (get-in @page-state [:selected])]
    (+busy page-state)
    (commodities/save (cond-> commodity
                        (not= "stock" (:type commodity)) (dissoc commodity :exchange))
                      (fn []
                        (swap! page-state #(-> %
                                               -busy
                                               (dissoc :selected)))
                        (load-commodities page-state))
                      (handle-error page-state "Unable to save the commodity: %s"))))

(defn- commodity-form
  [page-state]
  (let [commodity (r/cursor page-state [:selected])
        types (make-reaction #(if (and (:id @commodity)
                                       (= (:id @commodity)
                                          (get-in @current-entity [:settings :default-commodity-id])))
                                ["currency"]
                                ["currency" "stock" "fund"]))
        busy? (busy page-state)]
    (fn []
      [:div#commodity-form.card {:class (when-not @commodity "d-none")}
       [:div.card-header [:strong (str (if (:id @commodity) "Edit" "New")) " Commodity"]]
       [:div.card-body
        [forms/select-field commodity [:type] @types]
        (when (= "stock" (:type @commodity))
          [forms/select-field commodity [:exchange] ["nyse" "nasdaq"]])
        [forms/text-field commodity [:symbol] {:validate [:required]}]
        [forms/text-field commodity [:name] {:validate [:required]}]]
       [:div.card-footer
        [bs/busy-button {:html {:class "btn-primary"
                                :title "Click here to save this commodity"
                                :on-click #(save-commodity page-state)}
                         :icon :check
                         :caption "Save"
                         :busy? busy?}]
        [bs/busy-button {:html {:class "btn-secondary ms-2"
                                :title "Click here to discontinue this edit operation."
                                      :on-click #(swap! page-state dissoc :selected)}
                         :icon :x
                         :caption "Cancel"
                         :busy? busy?}]]])))

(defn- delete
  [commodity page-state]
  (when (js/confirm (str "Are you sure you want to delete the commodity \"" (:name commodity) "\"?"))
    (+busy page-state)
    (commodities/delete commodity
                        (fn []
                          (-busy page-state)
                          (load-commodities page-state))
                        (handle-error page-state "Unable to delete the commodity: %s"))))

(defn- truncate
  ([value] (truncate value 20))
  ([value length]
   (if (> length (count value))
     value
     [:span {:title value}
      (str
       (apply str (take (- length 3) value))
       "...")])))

(defn- init-price-loading
  [page-state]
  (let [commodity (:prices-commodity @page-state)
        {:keys [fetch-ch result-ch ctl-ch]} (->> (dates/desc-ranges (:earliest-price commodity)
                                                                    (:latest-price commodity)
                                                                    (t/years 1))
                                                 (map vec)
                                                 load-in-chunks)]
    (swap! page-state assoc :ctl-chan ctl-ch)
    (go-loop [rng (<! fetch-ch)]
             (if rng
               (do
                 (prices/search {:trade-date rng
                                 :commodity-id (:id commodity)}
                                (fn [prices]
                                  (swap! page-state update-in [:prices] (fnil concat []) prices)
                                  (go (>! result-ch prices)))
                                (notify/danger-fn "Unable to fetch the prices: %s"))
                 (recur (<! fetch-ch)))
               (swap! page-state assoc :all-prices-fetched? true)))
    (go (>! ctl-ch :fetch))))

(defn- commodity-row
  [{price :most-recent-price :as commodity} page-state]
  (let  [default? (= (:id commodity)
                     (get-in @current-entity [:settings :default-commodity-id]))]
    ^{:key (:id commodity)}
    [:tr
     [:td (truncate (:name commodity))]
     [:td.d-lg-table-cell.d-none (:symbol commodity)]
     [:td.d-lg-table-cell.d-none (:exchange commodity)]
     [:td.d-lg-table-cell.d-none.text-end {:title (format-date (:trade-date price))} (currency-format (:price price))]
     [:td.text-end
      [:div.btn-group
       [:button.btn.btn-light.btn-sm {:title "Click here to edit this commodity."
                                     :on-click (fn []
                                                 (swap! page-state #(-> %
                                                                        (dissoc :prices-commodity)
                                                                        (assoc :selected commodity)))
                                                 (html/set-focus "type"))}
        (bs/icon :pencil {:size :small})]
       [:button.btn.btn-light.btn-sm {:title "Click here to view prices for this commodity."
                                     :disabled default?
                                     :on-click (fn []
                                                 (swap! page-state #(-> %
                                                                        +busy
                                                                        (dissoc
                                                                          :selected
                                                                          :prices
                                                                          :prices-commodity
                                                                          :all-prices-fetched?)))
                                                 (js/setTimeout (fn []
                                                                  (swap! page-state assoc
                                                                              :prices-commodity commodity)
                                                                  (init-price-loading page-state)
                                                                  (-busy page-state))
                                                                100))}
        (bs/icon :collection {:size :small})]
       [:button.btn.btn-danger.btn-sm {:title "Click here to delete this commodity."
                                       :disabled default?
                                       :on-click #(delete commodity page-state)}
        (bs/icon :x-circle {:size :small})]]]]))

(defn- match-commodity
  [pattern]
  (if pattern
    (fn [commodity]
      (some #(re-find pattern (% commodity))
            [:name :symbol]))
    (constantly true)))

(defn- pagination
  [page-state]
  (let [commodities (r/cursor page-state [:commodities])
        com-count (make-reaction #(count @commodities))
        page-size (r/cursor page-state [:page-size])
        page-count (make-reaction #(.ceil js/Math (/ @com-count @page-size)))
        max-index (make-reaction #(- @page-count 1))
        page-index (r/cursor page-state [:page-index])]
    (fn []
      [:ul.pagination {:class (when (<= @com-count @page-size) "d-none")}
       [:li.page-item
        [:a.page-link {:href "#"
                       :on-click #(swap! page-state assoc :page-index 0)}
        (bs/icon :chevron-bar-left {:size :small})]]
       [:li.page-item
        [:a.page-link {:href "#"
                       :on-click #(swap! page-state update-in [:page-index] (fmin dec 0))}
        (bs/icon :chevron-left {:size :small})]]
       [:li.page-item
        [:a.page-link {:href "#"}
         (+ 1 @page-index)
         " of "
         @page-count]]
       [:li.page-item
        [:a.page-link {:href "#"
                       :on-click #(swap! page-state update-in [:page-index] (fmax inc @max-index))}
        (bs/icon :chevron-right {:size :small})]]
       [:li.page-item
        [:a.page-link {:href "#"
                       :on-click #(swap! page-state assoc :page-index @max-index)}
        (bs/icon :chevron-bar-right {:size :small})]]])))

(defn- commodities-table
  [page-state]
  (let [commodities (r/cursor page-state [:commodities])
        page-size (r/cursor page-state [:page-size])
        page-index (r/cursor page-state [:page-index])
        search-term (r/cursor page-state [:search-term])
        selected (r/cursor page-state [:selected])
        busy? (busy page-state)
        pattern (make-reaction #(when (and @search-term
                                           (< 2
                                              (count @search-term)))
                                  (re-pattern @search-term)))
        prices-selected (r/cursor page-state [:prices-commodity])
        hide? (make-reaction #(or @selected @prices-selected))]
    (fn []
      [:div {:class (when @hide? "d-none d-lg-block")}
       [:div
        [:div.input-group
         [:span.input-group-text
          (bs/icon :search {:size :small})]
         [forms/text-input page-state [:search-term]]
         [:div.input-group-append {:class (when-not @search-term "d-none")
                                   :on-click #(swap! page-state dissoc :search-term)}
          [:button.btn.btn-secondary
           (bs/icon :x {:size :small})]]]]
       [:table.table.table-striped.table-hover
        [:thead
         [:tr
          [:th "Name"]
          [:th.d-lg-table-cell.d-none "Symbol"]
          [:th.d-lg-table-cell.d-none "Exchange"]
          [:th.d-lg-table-cell.d-none.text-end "Latest Price"]
          [:th (html/space)]]]
        [:tbody
         (if @commodities
           (->> @commodities
                (filter (match-commodity @pattern))
                (drop (* @page-size @page-index))
                (take @page-size)
                (map #(commodity-row % page-state))
                doall)
           [:tr
            [:td.text-center {:col-span 5} [bs/spinner]]])]]
       [pagination page-state]
       [bs/busy-button {:html {:class "btn-primary"
                               :title "Click here to add a new commodity."
                               :on-click (fn []
                                           (swap! page-state
                                                  assoc
                                                  :selected
                                                  {:entity-id (:id @current-entity)
                                                   :type "stock"
                                                   :exchange "nyse"})
                                           (html/set-focus "type"))}
                        :busy? busy?
                        :icon :plus
                        :caption "Add"
                        :disabled? selected}]])))

(defn- delete-price
  [price page-state]
  (when (js/confirm (str "Are you sure you want to delete the price from " (format-date (:trade-date price)) "?"))
    (+busy page-state)
    (prices/delete price
                   (fn []
                     (swap! page-state
                            #(-> %
                                 -busy
                                 (update-in
                                   [:prices]
                                   (fn [prices]
                                     (remove (fn [p] (= (:id price) (:id p)))
                                             prices))))))
                   (handle-error page-state "Unable to delete the price: %s"))))

(defn- price-row
  [price page-state]
  ^{:key (str "price-row-" (:id price))}
  [:tr
   [:td.text-end (format-date (:trade-date price))]
   [:td.text-end (currency-format (:price price))]
   [:td
    [:div.btn-group
     [:button.btn.btn-light.btn-sm {:title "Click here to edit this price."
                                   :on-click (fn []
                                               (swap! page-state assoc :selected-price price)
                                               (html/set-focus "trade-date"))}
      (bs/icon :pencil {:size :small})]
     [:button.btn.btn-danger.btn-sm {:title "Click here to remove this price."
                                     :on-click #(delete-price price page-state)}
      (bs/icon :x-circle {:size :small})]]]])

(defn- price-list
  [page-state]
  (let [prices (r/cursor page-state [:prices])
        commodity (r/cursor page-state [:prices-commodity])
        all-prices-fetched? (r/cursor page-state [:all-prices-fetched?])
        ctl-chan (r/cursor page-state [:prices-ctl-chan])
        busy? (busy page-state)]
    (fn []
      [:div.card {:class (when-not @commodity "d-none")}
       [:div.card-header [:strong (str (:name @commodity) " Prices")]]
       [:div#prices-container {:style {:max-height "40em" :overflow "auto"}}
        [:table.table.table-striped.table-hover
         [:thead
          [:tr
           [:th.text-end "Trade Date"]
           [:th.text-end "Price"]
           [:th (html/space)]]]
         [:tbody
          (if @prices
            (doall (map #(price-row % page-state) @prices))
            [:tr [:td {:col-span 3} [:span.inline-status "Loading..."]]])]]]
       [:div.card-footer.d-flex.align-items-center
        [bs/busy-button {:html {:class "btn-primary"
                                :title "Click here to add a new price for this commodity."
                                :on-click (fn []
                                            (swap! page-state
                                                   assoc
                                                   :selected-price
                                                   {:commodity-id (:id @commodity)
                                                    :trade-date (t/today)})
                                            (html/set-focus "trade-date"))}
                         :icon :plus
                         :caption "Add"
                         :busy? busy?}]
        [bs/busy-button {:html {:class "btn-secondary ms-2"
                                :on-click #(swap! page-state dissoc :prices-commodity)}
                         :icon :x
                         :caption "Close"
                         :busy? busy?}]
        [:span.ms-auto
         [load-on-scroll {:target "prices-container"
                          :all-items-fetched? @all-prices-fetched?
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
    (+busy page-state)
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
                                            -busy
                                            (dissoc :selected-price)
                                            (update-in [:prices] update-fn)))))
                 (handle-error page-state "Unable to save the price: %s"))))

(defn- price-form
  [page-state]
  (let [price (r/cursor page-state [:selected-price])
        busy? (busy page-state)]
    (fn []
      [:div.card.mt-2 {:class (when-not @price "d-none")}
       [:div.card-header [:strong (str (if  (:id @price) "Edit" "New") " Price")]]
       [:div.card-body
        [forms/date-field price [:trade-date]]
        [forms/decimal-field price [:price]]]
       [:div.card-footer
        [bs/busy-button {:html {:class "btn-primary"
                                :title "Click here to save this price."
                                :on-click #(save-price page-state)}
                         :icon :check
                         :caption "Save"
                         :busy? busy?}]
        [bs/busy-button {:html {:class "btn-secondary ms-2"
                                :title "Click here to cancel this update."
                                :on-click #(swap! page-state dissoc :selected-price)}
                         :icon :x
                         :caption "Cancel"
                         :busy? busy?}]]])))

(defn- index []
  (let [page-state (r/atom {:page-size 10
                            :page-index 0
                            :prices-ctl-chan (chan)})]
    (load-commodities page-state)
    (add-watch current-entity ::index (fn [& _]
                                        (load-commodities page-state)))
    (fn []
      [:<>
       [:h1.mt-3 "Commodities"]
       [:div.row
        [:div.col-md-8
         [commodities-table page-state]]
        [:div.col-md-4
         [commodity-form page-state]
         [price-list page-state]
         [price-form page-state]]]])))

(secretary/defroute "/commodities" []
  (swap! app-state assoc :page #'index))
