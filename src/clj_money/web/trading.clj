(ns clj-money.web.trading
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [clj-time.core :as t]
            [clj-money.web.shared :refer :all]
            [clj-money.web.money-shared :refer [budget-monitors]]
            [clj-money.coercion :as coercion]
            [clj-money.validation :as validation]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.commodities :as commodities]
            [clj-money.trading :as trading]))

(defmacro with-trading-layout
  [page-title entity-id options & content]
  `(with-layout
     ~page-title (assoc ~options :side-bar (budget-monitors ~entity-id))
     ~@content))

(defn- commodity-options
  [entity-id]
  (map #(vector :option
                {:value (:id %)}
                (format "%s (%s)" (:name %) (:symbol %)))
       (commodities/select-by-entity-id (env :db) entity-id)))

(defn- form-fields
  [account trade]
  (html
    (date-input-field trade
                      :trade-date
                      {:autofocus true})
    (select-field trade
                  :commodity-id
                  (commodity-options (:entity-id account)))
    (number-input-field trade :shares)
    (number-input-field trade :value)
    [:input.btn.btn-primary {:type :submit
                             :value "Submit"
                             :title "Click here to save this trade."}]
    "&nbsp;"
    [:a.btn.btn-default {:href (format "/accounts/%s" (:id account))
                         :title "Click here to go back to the account page."}
     "Back"]))

(defn- params->trade
  [params]
  (cond-> {:trade-date (t/today)}
    (:commodity-id params)
    (assoc :commodity-id
           (Integer. (:commodity-id params)))

    (:shares params)
    (assoc :shares
           (bigdec (:shares params)))))

(defn new-purchase
  "Renders the form that allows the user to record a commodity purchase"
  ([{params :params :as req}]
   (new-purchase req (params->trade params)))
  ([{params :params} purchase]
   (let [account (accounts/find-by-id (env :db) (Integer. (:account-id params)))]
     (with-trading-layout "New Purchase" (:entity-id account) {}
       [:div.col-md-6
        [:form {:action (format "/accounts/%s/purchases" (:id account))
                :method :post}
         (form-fields account purchase)]]))))

(defn purchase
  [{params :params :as req}]
  (let [result (trading/buy (env :db) (select-keys params [:account-id
                                                           :commodity-id
                                                           :shares
                                                           :value
                                                           :trade-date]))]
    (if (validation/has-error? result)
      (new-purchase req result)
      (redirect (format "/accounts/%s" (:account-id result))))))

(defn new-sale
  ([{params :params :as req}]
   (new-sale req (params->trade params)))
  ([{params :params} sale]
   (let [account (accounts/find-by-id (env :db) (Integer. (:account-id params)))]
     (with-trading-layout "New Sale" (:entity-id account) {}
       [:div.col-md-6
        [:form {:action (format "/accounts/%s/sales" (:id account))
                :method :post}
         (form-fields account sale)]]))))

(defn sell
  [req]
  "sell")
