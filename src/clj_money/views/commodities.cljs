(ns clj-money.views.commodities
  (:require [cljs.core.async :refer [chan <! >! go go-loop]]
            [cljs.pprint :refer [pprint]]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [reagent.format :refer [currency-format]]
            [secretary.core :as secretary :include-macros true]
            [cljs-time.core :as t]
            [dgknght.app-lib.core :refer [fmin
                                          fmax]]
            [dgknght.app-lib.web :refer [format-date]]
            [dgknght.app-lib.dom :refer [set-focus]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.forms-validation :as v]
            [clj-money.util :as util :refer [id=]]
            [clj-money.icons :refer [icon]]
            [clj-money.dates :as dates]
            [clj-money.components :refer [button
                                          load-in-chunks
                                          load-on-scroll]]
            [clj-money.commodities :as cmm]
            [clj-money.api.commodities :as commodities]
            [clj-money.api.prices :as prices]
            [clj-money.state :refer [app-state
                                     current-entity
                                     +busy
                                     -busy]]))

(defn- receive-commodities
  [page-state]
  (fn [commodities]
    (swap! page-state
           #(-> %
                (dissoc :prices-commodity)
                (assoc :commodities (sort-by :commodity/name commodities))))))

(defn- load-commodities
  [page-state]
  (+busy)
  (commodities/select {}
    :callback -busy
    :on-success (receive-commodities page-state)))

(defn- save-commodity
  [page-state]
  (+busy)
  (-> (get-in @page-state [:selected])
      (update-in [:commodity/type] keyword)
      (update-in [:commodity/exchange] keyword)
      (commodities/save :callback -busy
                        :on-success (fn []
                                      (swap! page-state dissoc :selected)
                                      (load-commodities page-state)))))

(defn- commodity-form
  [page-state]
  (let [commodity (r/cursor page-state [:selected])
        types (make-reaction #(if (and (:id @commodity)
                                       (= (:id @commodity)
                                          (get-in @current-entity [:entity/settings :settings/default-commodity-id])))
                                ["currency"]
                                ["currency" "stock" "fund"]))]
    (fn []
      (when @commodity
        [:form {:on-submit (fn [e]
                             (.preventDefault e)
                             (v/validate commodity)
                             (when (v/valid? commodity)
                               (save-commodity page-state)))
                :no-validate true}
         [:div#commodity-form.card {:class (when-not @commodity "d-none")}
          [:div.card-header [:strong (str (if (:id @commodity) "Edit" "New")) " Commodity"]]
          [:div.card-body
           [forms/select-field commodity [:commodity/type] @types {:validations #{::v/required}}]
           [forms/select-field commodity [:commodity/exchange] ["" "amex" "nasdaq" "nyse" "otc"]]
           [forms/text-field commodity [:commodity/symbol] {:validations #{::v/required}}]
           [forms/text-field commodity [:commodity/name] {:validations #{::v/required}}]
           [forms/checkbox-field commodity [:commodity/price-config :price-config/enabled] {:caption "Download prices"}]]
          [:div.card-footer
           [button {:html {:class "btn-primary"
                           :type :submit
                           :title "Click here to save this commodity"}
                    :icon :check
                    :caption "Save"}]
           [button {:html {:class "btn-secondary ms-2"
                           :title "Click here to discontinue this edit operation."
                           :type :button
                           :on-click #(swap! page-state dissoc :selected)}
                    :icon :x
                    :caption "Cancel"}]]]]))))

(defn- delete
  [commodity page-state]
  (when (js/confirm (str "Are you sure you want to delete the commodity \"" (:commodity/name commodity) "\"?"))
    (+busy)
    (commodities/delete
      commodity
      :callback -busy
      :on-success #(load-commodities page-state))))

(defn- truncate
  ([value] (truncate value 20))
  ([value length]
   (if (> length (count value))
     value
     [:span {:title value}
      (str
       (apply str (take (- length 3) value))
       "...")])))

(defn- fetch-prices
  [xf]
  (completing
    (fn [ch criteria]
      (if criteria
        (prices/select criteria :on-success #(xf ch %))
        (xf ch [])))))

(defn- init-price-loading
  [page-state]
  (let [{:as commodity
         [start end] :commodity/price-date-range}
        (:prices-commodity @page-state)

        {:keys [items-ch ctl-ch]}
        (->> (dates/desc-ranges start
                                end
                                (t/years 1))
             (load-in-chunks {:fetch-xf (comp (map (fn [[start end :as range]]
                                                     (when (seq range)
                                                       {:price/commodity (util/->model-ref commodity)
                                                        :price/trade-date [:between> start end]})))
                                              fetch-prices)}))]
    (swap! page-state assoc :ctl-chan ctl-ch)
    (go-loop [prices (<! items-ch)]
             (if prices
               (do
                 (swap! page-state update-in [:prices] (fnil concat []) prices)
                 (recur (<! items-ch)))
               (swap! page-state #(-> %
                                      (update-in [:prices] (fnil identity []))
                                      (assoc :all-prices-fetched? true)))))
    (go (>! ctl-ch :fetch))))

(defn- select-prices-commodity
  [page-state commodity]
  (+busy)
  (swap! page-state
         #(-> %
              (dissoc :selected
                      :prices
                      :all-prices-fetched?)
              (assoc :prices-commodity commodity)))
  (js/setTimeout
    (fn []
      (if (:commodity/price-date-range commodity)
        (init-price-loading page-state)
        (swap! page-state assoc
               :prices []
               :all-prices-fetched? true))
      (-busy))
    100))

(defn- commodity-row
  [{:commodity/keys [latest-price
                     most-recent-price
                     symbol
                     exchange]
    :as commodity}
   page-state]
  (let  [default? (= (:id commodity)
                     (get-in @current-entity [:entity/settings
                                              :settings/default-commodity-id]))]
    ^{:key (:id commodity)}
    [:tr
     [:td (truncate (:commodity/name commodity))]
     [:td.d-lg-table-cell.d-none symbol]
     [:td.d-lg-table-cell.d-none exchange]
     [:td.d-lg-table-cell.d-none.text-end (currency-format (:price most-recent-price))]
     [:td.d-lg-table-cell.d-none.text-end (format-date latest-price)]
     [:td.text-end
      [:div.btn-group
       [:button.btn.btn-secondary.btn-sm
        {:title "Click here to edit this commodity."
         :on-click (fn []
                     (swap! page-state #(-> %
                                            (dissoc :prices-commodity)
                                            (assoc :selected commodity)))
                     (set-focus "type"))}
        (icon :pencil :size :small)]
       [:button.btn.btn-secondary.btn-sm
        {:title "Click here to view prices for this commodity."
         :disabled default?
         :on-click #(select-prices-commodity page-state
                                             commodity)}
        (icon :collection :size :small)]
       [:button.btn.btn-danger.btn-sm
        {:title "Click here to delete this commodity."
         :disabled default?
         :on-click #(delete commodity page-state)}
        (icon :x-circle :size :small)]]]]))

(defn- pagination
  [page-state collection]
  (let [com-count (make-reaction #(count @collection))
        page-size (r/cursor page-state [:page-size])
        page-count (make-reaction #(.ceil js/Math (/ @com-count @page-size)))
        max-index (make-reaction #(- @page-count 1))
        page-index (r/cursor page-state [:page-index])]
    (fn []
      [:ul.pagination {:class (when (<= @com-count @page-size) "d-none")}
       [:li.page-item
        [:a.page-link {:href "#"
                       :on-click #(swap! page-state assoc :page-index 0)}
         (icon :chevron-bar-left :size :small)]]
       [:li.page-item
        [:a.page-link {:href "#"
                       :on-click #(swap! page-state update-in [:page-index] (fmin dec 0))}
         (icon :chevron-left :size :small)]]
       [:li.page-item
        [:a.page-link {:href "#"}
         (+ 1 @page-index)
         " of "
         @page-count]]
       [:li.page-item
        [:a.page-link {:href "#"
                       :on-click #(swap! page-state update-in [:page-index] (fmax inc @max-index))}
         (icon :chevron-right :size :small)]]
       [:li.page-item
        [:a.page-link {:href "#"
                       :on-click #(swap! page-state assoc :page-index @max-index)}
         (icon :chevron-bar-right :size :small)]]])))

(defn- price-download-enabled?
  [{:commodity/keys [price-config]}]
  (:price-config/enabled price-config))

(defn- download-prices
  [page-state]
  (when-let [commodity-ids (->> (:commodities @page-state)
                                (filter (every-pred cmm/has-shares? price-download-enabled?))
                                (map :id)
                                seq)]
    (+busy)
    (prices/fetch commodity-ids
                  :callback -busy
                  :on-success #(load-commodities page-state))))

(defn- recent? []
  (let [an-hour-ago (t/minus (t/now) (t/hours 1))]
    (constantly false)
    (fn [{:commodity/keys [created-at]}]
      (t/before? an-hour-ago created-at))))

(defn- match-fn
  [hide-zero-shares? search-term]
  (apply every-pred
         (cond-> [(cmm/matches-search? search-term)]
           hide-zero-shares? (conj (some-fn cmm/has-shares?
                                            (recent?))))))

(defn- commodities-table
  [page-state]
  (let [commodities (r/cursor page-state [:commodities])
        hide-zero-shares? (r/cursor page-state [:hide-zero-shares?])
        search-term (r/cursor page-state [:search-term])
        match? (make-reaction #(match-fn @hide-zero-shares?
                                        @search-term))
        filtered (make-reaction #(filter @match? @commodities))
        page-size (r/cursor page-state [:page-size])
        page-index (r/cursor page-state [:page-index])
        current-page (make-reaction #(->> @filtered
                                          (drop (* @page-size @page-index))
                                          (take @page-size)))
        selected (r/cursor page-state [:selected])
        prices-selected (r/cursor page-state [:prices-commodity])
        hide? (make-reaction #(or @selected @prices-selected))]
    (fn []
      [:div {:class (when @hide? "d-none d-lg-block")}
       [:div
        [:div.input-group
         [:span.input-group-text
          (icon :search :size :small)]
         [forms/text-input page-state [:search-term]]
         [:button.btn.btn-secondary
          {:class (when-not @search-term "d-none")
           :on-click #(swap! page-state dissoc :search-term)}(icon :x)]]]
       [:table.table.table-striped.table-hover
        [:thead
         [:tr
          [:th "Name"]
          [:th.d-lg-table-cell.d-none "Symbol"]
          [:th.d-lg-table-cell.d-none "Exchange"]
          [:th.d-lg-table-cell.d-none.text-end "Latest Price"]
          [:th.d-lg-table-cell.d-none.text-end "Price Date"]
          [:th (html/space)]]]
        [:tbody
         (if @commodities
           (->> @current-page
                (map #(commodity-row % page-state))
                doall)
           [:tr
            [:td.text-center {:col-span 6} [bs/spinner]]])]]
       [pagination page-state filtered]
       [button {:html {:class "btn-primary"
                       :title "Click here to add a new commodity."
                       :on-click (fn []
                                   (swap! page-state
                                          assoc
                                          :selected
                                          #:commodity{:entity @current-entity
                                                      :type "stock"
                                                      :exchange "nyse"
                                                      :price-config {:price-config/enabled true}})
                                   (set-focus "type"))}
                :icon :plus
                :caption "Add"
                :disabled? selected}]
       [button {:html {:class "btn-secondary ms-2"
                       :title "Click here to download recent prices for each commodity."
                       :on-click #(download-prices page-state)}
                :icon :download
                :disabled? selected
                :caption "Fetch Prices"}]])))

(defn- post-delete-price
  [page-state]
  (fn [price]
    (swap! page-state
           update-in
           [:prices]
           (fn [prices]
             (remove #(id= price %) prices)))))

(defn- delete-price
  [price page-state]
  (when (js/confirm (str "Are you sure you want to delete the price from " (format-date (:trade-date price)) "?"))
    (+busy)
    (prices/delete price
                   :callback -busy
                   :on-success (post-delete-price page-state))))

(defn- price-row
  [{:as p :price/keys [trade-date price]} page-state]
  ^{:key (str "price-row-" (:id p))}
  [:tr
   [:td.text-end (format-date trade-date)]
   [:td.text-end (currency-format price)]
   [:td
    [:div.btn-group
     [:button.btn.btn-secondary.btn-sm
      {:title "Click here to edit this price."
       :on-click (fn []
                   (swap! page-state assoc :selected-price p)
                   (set-focus "trade-date"))}
      (icon :pencil :size :small)]
     [:button.btn.btn-danger.btn-sm {:title "Click here to remove this price."
                                     :on-click #(delete-price p page-state)}
      (icon :x-circle :size :small)]]]])

(defn- prices-table
  [page-state]
  (let [prices (r/cursor page-state [:prices])]
    (fn []
      [:table.table.table-striped.table-hover
       [:thead
        [:tr
         [:th.text-end "Trade Date"]
         [:th.text-end "Price"]
         [:th (html/space)]]]
       [:tbody
        (cond
          (nil? @prices)
          [:tr
           [:td.text-center
            {:col-span 3}
            (bs/spinner {:size :small})]]

          (empty? @prices)
          [:tr
           [:td
            {:col-span 3}
            [:span.inline-status "No prices found."]]]

          :else
          (doall (map #(price-row % page-state) @prices)))]])))

(defn- price-list
  [page-state]
  (let [commodity (r/cursor page-state [:prices-commodity])
        price (r/cursor page-state [:selected-price])
        all-prices-fetched? (r/cursor page-state [:all-prices-fetched?])
        ctl-chan (r/cursor page-state [:prices-ctl-chan])]
    (fn []
      [:div.card {:class (cond
                           (nil? @commodity) "d-none"
                           @price "d-none d-lg-block")}
       [:div.card-header [:strong (str (:name @commodity) " Prices")]]
       [:div#prices-container {:style {:max-height "40em" :overflow "auto"}}
        [prices-table page-state]]
       [:div.card-footer.d-flex.align-items-center
        [:div.row
         [:div.col-auto
          [button {:html {:class "btn-primary"
                          :title "Click here to add a new price for this commodity."
                          :on-click (fn []
                                      (swap! page-state
                                             assoc
                                             :selected-price
                                             #:price{:commodity @commodity
                                                     :trade-date (t/today)})
                                      (set-focus "trade-date"))}
                   :icon :plus
                   :caption "Add"}]
          [button {:html {:class "btn-secondary ms-2"
                          :on-click #(swap! page-state dissoc :prices-commodity)}
                   :icon :x
                   :caption "Close"}]]
         [:div.col-auto
          [:span.ms-auto
           [load-on-scroll {:target "prices-container"
                            :all-items-fetched? @all-prices-fetched?
                            :load-fn #(go (>! @ctl-chan :fetch))}]]]]]])))

(defn- post-save-price
  [page-state]
  (fn [price]
    (swap! page-state
           #(-> %
                (dissoc :selected-price)
                (update-in [:prices]
                           (fn [prices]
                             (util/upsert-into price
                                               {:sort-key :price/trade-date
                                                :comp t/after?}
                                               prices)))))))
(defn- save-price
  [page-state]
  (let [price (:selected-price @page-state)]
    (+busy)
    (prices/save price
                 :callback -busy
                 :on-success (post-save-price page-state))))

(defn- price-form
  [page-state]
  (let [price (r/cursor page-state [:selected-price])]
    (fn []
      (when @price
        [:form {:no-validate true
                :on-submit (fn [e]
                             (.preventDefault e)
                             (v/validate price)
                             (when (v/valid? price)
                               (save-price page-state)))}
         [:div.card.mt-2 {:class (when-not @price "d-none")}
          [:div.card-header [:strong (str (if  (:id @price) "Edit" "New") " Price")]]
          [:div.card-body
           [forms/date-field price [:price/trade-date] {:validations #{::v/required}}]
           [forms/decimal-field price [:price/value] {:validations #{::v/required}}]]
          [:div.card-footer
           [button {:html {:class "btn-primary"
                                   :title "Click here to save this price."
                                   :type :submit}
                            :icon :check
                            :caption "Save"}]
           [button {:html {:class "btn-secondary ms-2"
                           :title "Click here to cancel this update."
                           :type :button
                           :on-click #(swap! page-state dissoc :selected-price)}
                    :icon :x
                    :caption "Cancel"}]]]]))))

(defn- filter-container
  [page-state]
  [:div#filter.offcanvas.offcanvas-end {:tab-index -1}
   [:div.offcanvas-header
    [:h3.off-canvas-title "Filter"]
    [:button.btn-close.text-reset {:data-bs-dismiss "offcanvas"
                                   :aria-label "Close"}]]
   [:div.offcanvas-body
    [:div.form-check.mb-1
        [forms/checkbox-input
         page-state
         [:hide-zero-shares?]]
        [:label.form-check-label {:for "hide-zero-shares"}
         "Hide zero shares"]]]])

(defn- index []
  (let [page-state (r/atom {:page-size 10
                            :page-index 0
                            :prices-ctl-chan (chan)
                            :hide-zero-shares? true})]
    (load-commodities page-state)
    (add-watch current-entity ::index (fn [& _]
                                        (load-commodities page-state)))
    (fn []
      [:<>
       [:div.row
        [:div.col-md-8.d-flex.justify-content-between.align-items-center
         [:h1.mt-3 "Commodities"]
         [:button.btn.btn-dark {:type :button
                                :data-bs-toggle "offcanvas"
                                :data-bs-target "#filter"
                                :aria-controls "filter"}
          (icon :funnel :size :small)]]]
       [filter-container page-state]
       [:div.row
        [:div.col-md-8
         [commodities-table page-state]]
        [:div.col-md-4
         [commodity-form page-state]
         [price-list page-state]
         [price-form page-state]]]])))

(secretary/defroute "/commodities" []
  (swap! app-state assoc :page #'index))
