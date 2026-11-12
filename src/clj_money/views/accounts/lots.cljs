(ns clj-money.views.accounts.lots
  "The lots of a tradable account (the shares of one commodity held in an
  investment account), with their gains and losses at the latest price.
  Loads the lots and the commodity's latest price itself when it's mounted:
  [lots-table account]"
  (:require [goog.string :as gstr]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [reagent.format :refer [currency-format]]
            [dgknght.app-lib.web :refer [format-percent
                                         format-date
                                         format-decimal]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.decimal :as decimal]
            [clj-money.dates :refer [serialize-local-date]]
            [clj-money.components :refer [audit-history-popover]]
            [clj-money.api.lots :as lots]
            [clj-money.api.commodities :as commodities]
            [clj-money.api.audit :as audit]))

(defn- load-lot-audit!
  [state lot]
  (audit/select lot :lot/shares-purchased
                :on-success
                #(swap! state assoc-in [:audit-histories (:id lot)] %)))

(defn- load-lots
  [state {:account/keys [parent commodity]}]
  (lots/select
    #:lot{:account parent
          :commodity commodity}
    :on-success (fn [lots]
                  (swap! state assoc :lots lots)
                  (doseq [lot lots]
                    (load-lot-audit! state lot)))))

(defn- load-latest-price
  [state {:account/keys [commodity]}]
  (commodities/find
    commodity
    :on-success #(swap! state
                        assoc
                        :latest-price
                        (:commodity/most-recent-price %))))

(defn- toggle-lot-audit!
  [state lot]
  (swap! state update :audit-expanded
         (fn [expanded]
           (let [s (or expanded #{})
                 id (:id lot)]
             (if (s id) (disj s id) (conj s id))))))

(defn- lot-row
  [lot latest-price gain-loss state]
  (let [g-l (- (* (:price/value latest-price)
                  (:lot/shares-owned lot))
               (* (:lot/purchase-price lot)
                  (:lot/shares-owned lot)))
        id (:id lot)
        audit-history (get-in @state [:audit-histories id])
        expanded? (contains? (:audit-expanded @state) id)]
    ^{:key (str "lot-" id)}
    [:tr
     [:td.text-end (format-date (:lot/purchase-date lot))]
     [:td.text-end
      [:div.d-flex.justify-content-end.align-items-center
       [:div.d-flex.align-items-center
        [audit-history-popover
         audit-history
         expanded?
         #(toggle-lot-audit! state lot)]
        [:div.ms-2
         (format-decimal (:lot/shares-purchased lot) 4)]]]]
     [:td.text-end (format-decimal (:lot/shares-owned lot) 4)]
     [:td.text-end (format-decimal (:lot/purchase-price lot) 2)]
     [:td.text-end
      {:class (if (>= g-l 0M)
                "text-success"
                "text-danger")}
      (format-decimal g-l)]
     [:td.text-end
      {:class (if (>= gain-loss 0M)
                "text-success"
                "text-danger")}
      (format-percent (/ g-l
                         (* (:lot/shares-purchased lot)
                            (:lot/purchase-price lot)))
                      3)]]))

(defn lots-table
  [account]
  (let [state (r/atom {})
        lots (r/cursor state [:lots])
        held-lots (make-reaction
                    #(filter (comp pos? :lot/shares-owned)
                             @lots))
        latest-price (r/cursor state [:latest-price])
        total-shares (make-reaction #(->> @lots
                                          (map :lot/shares-owned)
                                          (reduce decimal/+)))
        total-value (make-reaction #(decimal/* (or @total-shares
                                                   (decimal/zero))
                                               (or (:price/value @latest-price)
                                                   (decimal/zero))))
        total-cost (make-reaction #(->> @lots
                                        (map (fn [{:lot/keys [purchase-price shares-owned]}]
                                               (* purchase-price shares-owned)))
                                        (reduce +)))
        gain-loss (make-reaction #(- @total-value @total-cost))]
    (load-lots state account)
    (load-latest-price state account)
    (fn []
      [:table.table.table-hover.table-borderless
       [:thead
        [:tr
         [:th.text-end "Purchase Date"]
         [:th.text-end "Shares Purchased"]
         [:th.text-end "Shares Owned"]
         [:th.text-end "Purchase Price"]
         [:th.text-end "Gn/Ls"]
         [:th.text-end "Gn/Ls %"]]]
       [:tbody
        (cond
          (nil? @lots)
          [:tr
           [:td.text-center {:col-span 6}
            [:div.d-flex.justify-content-center.m2
             [:div.spinner-border {:role :status}
              [:span.visually-hidden "Loading"]]]]]

          (seq @held-lots)
          (->> @held-lots
               (sort-by (comp serialize-local-date
                              :lot/purchase-date))
               (map #(lot-row % @latest-price @gain-loss state))
               doall)

          :else
          [:tr
           [:td.text-center.text-body-tertiary {:col-span 6}
            "No current lots for this commodity."]])]
       (when (seq @held-lots)
         [:tfoot
          [:tr
           [:td.text-end {:col-span 2}
            (when @latest-price
              (gstr/format "(%s as of %s)"
                           (currency-format (:price/value @latest-price))
                           (format-date (:price/trade-date @latest-price))))]
           [:td.text-end (format-decimal @total-shares 4)]
           [:td.text-end (currency-format @total-value)]
           [:td.text-end
            {:class (if (>= @gain-loss 0M) "text-success" "text-danger")}
            (currency-format @gain-loss)]
           [:td.text-end
            {:class (if (>= @gain-loss 0M) "text-success" "text-danger")}
            (format-percent (/ @gain-loss
                               @total-cost)
                            3)]
           [:td (html/space)]]])])))
