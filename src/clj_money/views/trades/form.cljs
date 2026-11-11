(ns clj-money.views.trades.form
  "The form for entering a trade: a purchase, a sale, a transfer of shares
  to another account, or a reinvested dividend.

  The caller owns an atom (or cursor) holding the trade, started with
  new-trade or new-dividend, and renders
  [trade-form trade :commodities ... :on-save ... :on-cancel ...].
  :commodities maps the ids of the commodities that can be traded to the
  commodities. The form saves the trade itself and passes the result to
  :on-save. When \"Save and add another\" is checked, it then starts the next
  trade in the atom, with the same date, accounts and commodity."
  (:require [clojure.string :as string]
            [cljs-time.core :as t]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [dgknght.app-lib.web :refer [format-decimal]]
            [dgknght.app-lib.inflection :refer [humanize]]
            [dgknght.app-lib.dom :refer [set-focus]]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.forms-validation :as v]
            [clj-money.icons :refer [icon-with-text]]
            [clj-money.state :refer [accounts
                                     accounts-by-id
                                     current-entity
                                     +busy
                                     -busy]]
            [clj-money.util :as util :refer [id=]]
            [clj-money.commodities :as cmdts]
            [clj-money.accounts :refer [find-by-path
                                        system-tagged?
                                        transfer-destination?]]
            [clj-money.transactions :refer [->trade-price
                                            trade->transfer]]
            [clj-money.api.trading :as trading]
            [clj-money.api.transfers :as transfers]))

(defn new-trade
  "Returns a new purchase made from the given account. When the account is a
  tradable one (the shares of a commodity held in an investment account), it's
  a purchase of that commodity, made from its parent."
  [account]
  (let [tradable? (system-tagged? account :tradable)]
    #:trade{:entity @current-entity
            :action :buy
            :date (t/today)
            :commodity-account (when tradable? account)
            ; The commodity field holds the commodity id
            :commodity (when tradable?
                         (get-in account [:account/commodity :id]))
            :account (if tradable?
                       (:account/parent account)
                       account)}))

(defn new-dividend
  "Returns a new dividend reinvested in the given account"
  [account]
  #:trade{:entity @current-entity
          :dividend? true
          :action :buy
          :date (t/today)
          :account account})

(defn- next-trade
  "Returns the trade to enter after the given one, when the user asked to add
  another"
  [{:trade/keys [dividend?] :as trade}]
  (select-keys trade
               (cond-> [:trade/entity
                        :trade/dividend?
                        :trade/action
                        :trade/date
                        :trade/account
                        :trade/fee-account]
                 dividend? (conj :trade/dividend-account)
                 (not dividend?) (conj :trade/commodity
                                       :trade/commodity-account))))

(defn- drop-hidden-field-rules
  "Validation rules stay in the model's metadata after their field is
  unmounted, so remove the rules for the fields that the selected action
  doesn't show"
  [trade]
  (let [hidden (if (= :transfer (:trade/action @trade))
                 [[:trade/value]]
                 [[:trade/to-account]])]
    (swap! trade
           vary-meta
           update-in
           [::v/validation ::v/rules]
           #(apply dissoc % hidden))))

(defn- shares-held
  "Returns the shares of the trade's commodity held in the trade's account,
  read from the balance of the commodity account beneath it"
  [{:trade/keys [account commodity]}]
  (when (and account commodity)
    (let [commodity-id (or (:id commodity) commodity)]
      (some (fn [{:account/keys [parent quantity] :as a}]
              (when (and (id= parent account)
                         (= commodity-id
                            (get-in a [:account/commodity :id])))
                (or quantity (decimal/zero))))
            @accounts))))

(defn- account-field
  [trade path & {:as opts}]
  [forms/typeahead-field
   trade
   path
   (merge {:search-fn (fn [input callback]
                        (->> @accounts
                             (find-by-path input)
                             callback))
           :caption-fn (comp (partial string/join "/") :account/path)
           :find-fn (fn [{:keys [id]} callback]
                      (callback (@accounts-by-id id)))}
          opts)])

(defn- save
  [trade options on-save]
  (drop-hidden-field-rules trade)
  (v/validate trade)
  (when (v/valid? trade)
    (let [saving @trade
          on-success (fn [result]
                       (on-save result)
                       (when (:repeat? @options)
                         (reset! trade (next-trade saving))
                         (set-focus "transaction-date")))]
      (+busy)
      (if (= :transfer (:trade/action saving))
        (transfers/create (trade->transfer saving)
                          :callback -busy
                          :on-success on-success)
        (trading/create saving
                        :callback -busy
                        :on-success on-success)))))

(defn- trade-fields
  [trade options _commodities]
  (let [dividend? (r/cursor trade [:trade/dividend?])
        transfer? (make-reaction #(= :transfer (:trade/action @trade)))
        held (make-reaction #(when @transfer? (shares-held @trade)))
        price (make-reaction #(->trade-price @trade))]
    (fn [_trade _options commodities]
      [:<>
       [forms/date-field trade [:trade/date] {:validations #{::v/required}}]
       (when-not @dividend?
         [forms/select-field
          trade
          [:trade/action]
          (map (juxt name humanize)
               [:buy :sell :transfer])
          {:transform-fn keyword
           :validations #{::v/required}}])
       (if @transfer?
         [:<>
          [forms/decimal-field trade [:trade/shares] {:validations #{::v/required}}]
          (when @held
            [:p#shares-held.form-text.mt-n2
             "Shares held: "
             (format-decimal @held {:fraction-digits 4})])
          [forms/typeahead-field
           trade
           [:trade/to-account]
           {:search-fn (fn [input callback]
                         (->> @accounts
                              (filter transfer-destination?)
                              (remove #(util/entity= % (:trade/account @trade)))
                              (find-by-path input)
                              callback))
            :caption-fn (comp (partial string/join "/") :account/path)
            :find-fn (fn [{:keys [id]} callback]
                       (callback (@accounts-by-id id)))
            :validations #{::v/required}}]]
         [:<>
          [:div.row
           [:div.col-md-4
            [forms/decimal-field trade [:trade/shares] {:validations #{::v/required}}]]
           [:div.col-md-4
            [forms/decimal-field trade [:trade/value] {:validations #{::v/required}}]]
           [:div.col-md-4.d-flex.flex-column
            [:span.mb-2 "Est. Price"]
            [:span.mb-3.ms-3 (when @price (format-decimal @price))]]]
          [:div.row
           [:div.col-md-4
            [forms/decimal-field trade [:trade/fee]]]
           [:div.col-md-4
            [account-field trade [:trade/fee-account]]]
           [:div.col-md-4.d-flex.align-items-end.checkbox-beside-input
            [forms/checkbox-field trade [:trade/value-includes-fee?] {:caption "Value includes fee"}]]]])
       [forms/typeahead-field
        trade
        [:trade/commodity]
        {:search-fn (fn [input callback]
                      (callback (cmdts/search input (vals commodities))))
         :caption-fn cmdts/description
         :value-fn :id
         :find-fn (fn [id callback]
                    (callback (get commodities id)))
         :validations #{::v/required}}]
       (when @dividend?
         [account-field trade [:trade/dividend-account]
          :validations #{::v/required}])
       [forms/checkbox-field
        options
        [:repeat?]
        {:caption "Save and add another"}]])))

(defn trade-form
  [trade & _opts]
  (let [options (r/atom {})
        dividend? (r/cursor trade [:trade/dividend?])]
    (fn [_trade & {:keys [commodities on-save on-cancel]}]
      [:<>
       [:h3 (if @dividend? "Reinvest Dividend" "New Trade")]
       [:form#trade-form.mt-3
        {:no-validate true
         :on-submit (fn [e]
                      (.preventDefault e)
                      (save trade options on-save))}
        [trade-fields trade options commodities]]
       [:div
        [:button.btn.btn-primary
         {:type :submit
          :form "trade-form"
          :title "Click here to record this trade"}
         (icon-with-text :check "Save")]
        [:button.btn.btn-secondary.ms-2
         {:on-click on-cancel
          :title "Click here to cancel this trade"}
         (icon-with-text :x "Cancel")]]])))
