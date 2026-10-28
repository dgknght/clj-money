(ns clj-money.ingestion.statement-reconciliation
  "Reconciles an account from a statement read by a model"
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [java-time.api :as t]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util :refer [id=]]
            [clj-money.images :as images]
            [clj-money.entities :as entities]
            [clj-money.entities.reconciliations :as recs]
            [clj-money.accounts :refer [trading?]]
            [clj-money.trading :as trading]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.statements :as stmts]
            [clj-money.ingestion.investment-statements :as inv]))

(defn- error-message
  [e]
  (if-let [errors (::v/errors (ex-data e))]
    (str "The statement could not be reconciled. "
         (->> (vals errors)
              flatten
              (filter string?)
              (str/join " ")))
    (ex-message e)))

(defn- resolve-account
  [{:keys [account]} given accounts-by-path]
  (or given
      (accounts-by-path account)
      (throw (ex-info "Unable to tell which account the statement is for."
                      {:account account}))))

(defn- ensure-after-last-reconciliation
  "Throws if the statement doesn't come after the last completed
  reconciliation, which is checked before anything is saved, so that nothing
  needs to be undone"
  [account {:keys [period-end]}]
  (when-let [last-completed (entities/find-by
                              #:reconciliation{:account (util/->entity-ref account)
                                               :status :completed}
                              {:sort [[:reconciliation/end-of-period :desc]]})]
    (when-not (t/after? period-end (:reconciliation/end-of-period last-completed))
      (throw (ex-info (format "The statement ends on %s, which is not after the last reconciliation, on %s."
                              period-end
                              (:reconciliation/end-of-period last-completed))
                      {:period-end period-end})))))

(defn- warnings
  [account {:keys [opening-balance]}]
  (let [previous (or (recs/previous-balance account) 0M)]
    (cond-> []
      (not (zero? (compare opening-balance previous)))
      (conj (format "The statement's opening balance, %s, doesn't match the balance of the last reconciliation, %s."
                    opening-balance
                    previous)))))

(defn- attach-statement
  "Attaches the statement to each of the transactions. This is done once
  everything else is saved: updating the transaction's count of attachments
  in the background could otherwise restore a transaction deleted because
  the reconciliation failed."
  [trxs image]
  (when (seq trxs)
    (entities/put-many (map #(hash-map :attachment/transaction (util/->entity-ref %)
                                       :attachment/image image
                                       :attachment/caption "Statement")
                            trxs))))

(defn- create-transactions
  "Saves a transaction for each statement line that matched nothing already
  recorded, and returns the items they add to the account"
  [lines account {:keys [entity accounts-by-path created]}]
  (when (seq lines)
    (let [saved (->> lines
                     (map #(stmts/make-trx % account entity accounts-by-path))
                     entities/put-many
                     (filter (util/entity-type? :transaction)))]
      (swap! created into saved)
      (->> saved
           (mapcat :transaction/items)
           (filter #(id= account (:transaction-item/account %)))))))

(defn- create-trades
  "Records each trade that matched nothing already recorded, and returns
  the items of its transactions"
  [trades account {:keys [accounts-by-path created]}]
  (let [trxs (into []
                   (mapcat (fn [{:keys [type] :as line}]
                             (let [trade (inv/make-trade line account accounts-by-path)
                                   trxs (:trade/transactions (if (= "sell" type)
                                                               (trading/sell trade)
                                                               (trading/buy trade)))]
                               (swap! created into trxs)
                               trxs)))
                   trades)]
    (mapcat :transaction/items trxs)))

(defn- reconcile-cash-account
  "Matches the lines to the items in the account, records the lines that
  match none, and saves the reconciliation. Returns any warnings."
  [statement account {:keys [image] :as opts}]
  (let [{:keys [matched unmatched]} (stmts/match-items
                                      (:transactions statement)
                                      (stmts/candidate-items account statement))
        new-items (create-transactions unmatched account opts)]
    (entities/put (stmts/make-reconciliation account
                                             statement
                                             (concat (map second matched)
                                                     new-items)
                                             :image image))
    []))

(defn- reconcile-trading-account
  "Matches the lines to the items in the trading account and its commodity
  accounts, records the lines that match none, and saves a reconciliation
  for the cash and for each commodity. Returns any warnings."
  [statement account {:keys [entity image] :as opts}]
  (let [lines (inv/resolve-lines (:transactions statement) entity)
        {:keys [items
                unmatched-trades
                unmatched-cash]} (inv/match-lines lines
                                                  account
                                                  (inv/commodity-accounts account)
                                                  statement)
        new-items (concat (create-trades unmatched-trades account opts)
                          (create-transactions unmatched-cash account opts))
        ; a purchase may have added a commodity account
        commodity-accounts (inv/commodity-accounts account)
        reconciled? (comp (set (cons (:id account)
                                     (map :id (vals commodity-accounts))))
                          :id
                          :transaction-item/account)
        {:keys [reconciliations warnings]} (inv/make-reconciliations
                                             account
                                             statement
                                             (filter reconciled?
                                                     (concat items new-items))
                                             commodity-accounts
                                             :image image)]
    (entities/put-many reconciliations)
    (concat (:warnings lines) warnings)))

(defn reconcile
  "Reads the statement uploaded with the ingestion, matches its lines to the
  account's transactions, creates transactions for the lines that match
  none, and saves the account's reconciliation, unfinished, for the user to
  complete. For a trading account, the trades are matched to the commodity
  accounts, and each of them is reconciled too. Returns the ingestion,
  complete, or failed with an error."
  [ingestion reader]
  (let [{:statement-ingestion/keys [entity image account] :as ingestion}
        (entities/put (assoc ingestion :statement-ingestion/status :processing))
        entity (entities/find entity)
        given (some-> account entities/find)
        created (atom [])]
    (try
      (let [by-path (stmts/accounts-by-path entity)
            raw (ing/read-statement reader
                                    (images/get (:image/uuid (entities/find image)))
                                    entity
                                    {:account-options (stmts/account-options entity
                                                                             :account given)})
            statement (stmts/normalize raw)
            account (resolve-account statement given by-path)
            _ (ensure-after-last-reconciliation account statement)
            account-warnings ((if (trading? account)
                                reconcile-trading-account
                                reconcile-cash-account)
                              statement
                              account
                              {:entity entity
                               :image image
                               :accounts-by-path by-path
                               :created created})]
        (attach-statement @created image)
        (entities/put (assoc ingestion
                             :statement-ingestion/status :complete
                             :statement-ingestion/account (util/->entity-ref account)
                             :statement-ingestion/statement raw
                             :statement-ingestion/warnings (vec (concat (warnings account statement)
                                                                        account-warnings)))))
      (catch Exception e
        (log/errorf e "[statement-ingestion] unable to read statement %s" (:id ingestion))
        ; the transactions were created for the reconciliation, so they go
        ; if it couldn't be saved. A trade is undone, restoring its lots,
        ; the latest first.
        (doseq [trx (reverse @created)]
          (some-> (entities/find trx) trading/delete-transaction))
        (entities/put (assoc ingestion
                             :statement-ingestion/status :failed
                             :statement-ingestion/error (error-message e)))))))
