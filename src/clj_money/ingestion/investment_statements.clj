(ns clj-money.ingestion.investment-statements
  "Reads investment account statements. The cash in the account is
  reconciled in the trading account itself, and each commodity, counted in
  shares, in its own account beneath it."
  (:require [dgknght.app-lib.core :refer [index-by]]
            [clj-money.util :as util :refer [id=]]
            [clj-money.entities :as ents]
            [clj-money.accounts :refer [tradable?]]
            [clj-money.ingestion.statements :as stmts]))

(def ^:private trade-types #{"buy" "sell" "reinvest"})

(def ^:private trade?
  (comp trade-types :type))

(defn commodity-accounts
  "Returns the accounts beneath the trading account that hold its
  commodities, indexed by the id of the commodity"
  [account]
  (->> (ents/select {:account/parent (util/->entity-ref account)})
       (filter tradable?)
       (index-by (comp :id :account/commodity))))

(defn- commodities-by-symbol
  [entity]
  (->> (ents/select {:commodity/entity (util/->entity-ref entity)})
       (index-by :commodity/symbol)))

(defn resolve-lines
  "Splits the statement lines into the :trades, each with its :commodity,
  and the :cash lines. A trade in a commodity the entity doesn't have is
  left out, with one of the :warnings."
  [lines entity]
  (let [commodities (commodities-by-symbol entity)
        {trades true cash false} (group-by (comp boolean trade?) lines)]
    (reduce (fn [result {:keys [symbol description] :as line}]
              (if-let [commodity (commodities symbol)]
                (update-in result [:trades] conj (assoc line :commodity commodity))
                (update-in result
                           [:warnings]
                           conj
                           (format "The commodity %s, traded in \"%s\", was not found, so the trade was not recorded."
                                   symbol
                                   description))))
            {:trades []
             :cash (vec cash)
             :warnings []}
            trades)))

(defn- share-change
  "Returns the change in the shares held for a trade line"
  [{:keys [type shares]}]
  (if (= "sell" type)
    (- shares)
    shares))

(defn- trade-match?
  "Returns a function that is true when the item records the line's trade:
  the same change in shares of the commodity, dated within a few days of
  the line"
  [commodity-accounts]
  (fn [{:keys [date commodity] :as line}
       {:transaction-item/keys [account polarized-quantity]
        :transaction/keys [transaction-date]}]
    (and (id= account (commodity-accounts (:id commodity)))
         (zero? (compare (share-change line) polarized-quantity))
         (<= (stmts/days-apart date transaction-date)
             stmts/match-window-days))))

(defn- remove-items
  [items to-remove]
  (let [ids (set (map :id to-remove))]
    (remove (comp ids :id) items)))

(defn- trade-cash-lines
  "Returns the cash movements of the matched trades, dated as they were
  recorded. A reinvestment is recorded as two: the dividend paid into the
  account, and the purchase made with it."
  [matched]
  (mapcat (fn [[{:keys [type] :as line} item]]
            (let [line (assoc line :date (:transaction/transaction-date item))]
              (if (= "reinvest" type)
                [line (update-in line [:amount] -)]
                [line])))
          matched))

(defn match-lines
  "Matches the trades to the items in the commodity accounts, and the cash
  lines to the items in the trading account. Returns a map with the :items
  that were matched, both the commodity items and the cash items of the
  trades, and the :unmatched-trades and :unmatched-cash lines."
  [{:keys [trades cash]} account commodity-accounts statement]
  (let [commodity-items (mapcat #(stmts/candidate-items % statement)
                                (vals commodity-accounts))
        cash-items (stmts/candidate-items account statement)
        {trade-matches :matched
         unmatched-trades :unmatched} (stmts/match-items
                                        trades
                                        commodity-items
                                        :match? (trade-match? commodity-accounts))
        trade-cash-items (->> cash-items
                              (stmts/match-items (trade-cash-lines trade-matches))
                              :matched
                              (map second))
        {cash-matches :matched
         unmatched-cash :unmatched} (stmts/match-items
                                      cash
                                      (remove-items cash-items trade-cash-items))]
    {:items (concat (map second trade-matches)
                    trade-cash-items
                    (map second cash-matches))
     :unmatched-trades unmatched-trades
     :unmatched-cash unmatched-cash}))

(defn make-trade
  "Returns the trade, awaiting the user's review, for a statement line that
  matched nothing already recorded. A reinvestment buys shares with the
  dividend, which is paid from the category chosen by the model."
  [{:keys [type date shares amount commodity] :as line}
   account
   accounts-by-path]
  (cond-> #:trade{:date date
                  :shares shares
                  :value (abs amount)
                  :commodity (util/->entity-ref commodity)
                  :account (util/->entity-ref account)
                  :transaction-attributes #:transaction{:source :ingestion
                                                        :review-status :pending}}
    (= "reinvest" type) (assoc :trade/dividend? true
                               :trade/dividend-account
                               (util/->entity-ref
                                 (stmts/other-account line
                                                      account
                                                      accounts-by-path)))))

(defn make-reconciliations
  "Returns the reconciliations for the statement: one for the cash in the
  trading account, with the statement's closing balance, and one for each
  commodity account with items on the statement or shares held at its end,
  with the shares held. A commodity missing from the holdings is no longer
  held. Returns a map with the :reconciliations and the :warnings for
  holdings that aren't in any of the commodity accounts."
  [account
   {:keys [holdings] :as statement}
   items
   commodity-accounts
   & {:keys [image]}]
  (let [commodities (commodities-by-symbol (:account/entity account))
        by-symbol (->> commodities
                       (keep (fn [[symbol {:keys [id]}]]
                               (when-let [a (commodity-accounts id)]
                                 [symbol a])))
                       (into {}))
        symbols (->> commodities
                     (map (fn [[symbol {:keys [id]}]]
                            [id symbol]))
                     (into {}))
        by-account (group-by (comp :id :transaction-item/account) items)
        held (->> holdings
                  (map (juxt :symbol :shares))
                  (into {}))
        reconciled (->> (keys held)
                        (keep by-symbol)
                        (concat (filter (comp by-account :id)
                                        (vals commodity-accounts)))
                        (index-by :id)
                        vals)]
    {:reconciliations
     (cons (stmts/make-reconciliation account
                                      statement
                                      (by-account (:id account))
                                      :image image)
           (map (fn [{:keys [id] :account/keys [commodity] :as commodity-account}]
                  (stmts/make-reconciliation
                    commodity-account
                    (assoc statement
                           :closing-balance
                           (get held (symbols (:id commodity)) 0M))
                    (by-account id)
                    :image image))
                reconciled))
     :warnings (->> (keys held)
                    (remove by-symbol)
                    (map #(format "The statement lists shares of %s, which isn't held in %s."
                                  %
                                  (:account/name account))))}))
