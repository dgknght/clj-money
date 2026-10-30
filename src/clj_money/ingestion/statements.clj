(ns clj-money.ingestion.statements
  "Reads account statements, matching the statement lines to the
  transactions already recorded, and recording those that are missing"
  (:require [clojure.string :as str]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [index-by]]
            [clj-money.util :as util]
            [clj-money.entities :as ents]
            [clj-money.entities.transaction-items :as trx-items]
            [clj-money.accounts :refer [nest
                                        unnest
                                        derive-action
                                        polarize-quantity
                                        trading?
                                        tradable?]]
            [clj-money.ingestion.text :refer [similarity]]))

(def ^:private statement-account-types #{:asset :liability})

(def ^:private leaf-account?
  (comp (some-fn nil? zero?)
        :child-count))

(defn- ->path
  [account]
  (str/join "/" (:account/path account)))

(defn- entity-accounts
  [entity]
  (->> (ents/select {:account/entity entity})
       nest
       unnest
       (remove :account/hidden)))

(defn accounts-by-path
  "Returns the entity's accounts, indexed by their full paths
  (e.g., \"Household/Groceries\")"
  [entity]
  (index-by ->path (entity-accounts entity)))

(defn- symbols
  "Returns the symbols of the commodities the entity trades"
  [entity]
  (->> (ents/select {:commodity/entity entity})
       (remove #(= :currency (:commodity/type %)))
       (mapv :commodity/symbol)
       sort
       vec))

(defn account-options
  "Returns the account paths the model may choose from for the account the
  statement is for, and for the other side of each transaction on it. When
  the account is already known, the model isn't asked to identify it. When
  the statement may be for an investment account, the symbols of the
  commodities the entity trades are included too, and the model is asked to
  read the trades and holdings."
  [{{:settings/keys [expense-accounts]} :entity/settings :as entity}
   & {:keys [account]}]
  (let [accounts (entity-accounts entity)
        expense? (if (seq expense-accounts)
                   (comp (set (map :id expense-accounts))
                         :id)
                   leaf-account?)
        category? (fn [{:account/keys [type] :as account}]
                    (case type
                      :expense (expense? account)
                      :equity false
                      (leaf-account? account)))]
    (cond-> {:statement-accounts (when-not account
                                   (->> accounts
                                        (filter (comp statement-account-types
                                                      :account/type))
                                        ; a commodity is on the statement for
                                        ; its trading account
                                        (remove tradable?)
                                        (mapv ->path)))
             :category-accounts (->> accounts
                                     (filter category?)
                                     (mapv ->path))}
      (if account
        (trading? account)
        (some trading? accounts))
      (assoc :symbols (symbols entity)))))

(defn- account-property
  [description paths]
  (cond-> {:type "string"
           :description description}
    (seq paths) (assoc :enum paths)))

(def ^:private line-types ["cash" "buy" "sell" "reinvest"])

(def ^:private investment-line-properties
  {:type {:type "string"
          :enum line-types
          :description "The kind of line: a trade of a commodity, or cash moving in or out of the account"}
   :symbol {:type ["string" "null"]
            :description "The ticker symbol of the commodity traded"}
   :shares {:type ["number" "null"]
            :description "The number of shares traded"}})

(def ^:private holdings-property
  {:type "array"
   :items {:type "object"
           :properties {:symbol {:type "string"}
                        :shares {:type "number"}}
           :required ["symbol" "shares"]}})

(defn- add-investment-properties
  "Adds what's read from an investment statement: the kind of each line,
  with the commodity and shares for a trade, and the holdings at the end of
  the period"
  [schema]
  (-> schema
      (update-in [:properties :transactions :items :properties]
                 merge
                 investment-line-properties)
      (update-in [:properties :transactions :items :required] conj "type")
      (assoc-in [:properties :holdings] holdings-property)
      (update-in [:required] conj "holdings")))

(defn schema
  "Returns a JSON schema describing the expected response, given the account
  paths the model may choose from, and, for a statement that may be for an
  investment account, the symbols of the commodities the entity trades."
  [{:keys [statement-accounts category-accounts symbols]}]
  (let [identify? (seq statement-accounts)]
    (cond-> {:type "object"
             :properties (cond-> {:institution {:type ["string" "null"]
                                                :description "The name of the bank or card issuer"}
                                  :period_start {:type "string"
                                                 :description "The first day of the statement period"}
                                  :period_end {:type "string"
                                               :description "The last day of the statement period"}
                                  :opening_balance {:type "number"}
                                  :closing_balance {:type "number"}
                                  :transactions {:type "array"
                                                 :items {:type "object"
                                                         :properties {:date {:type "string"}
                                                                      :description {:type "string"}
                                                                      :amount {:type "number"}
                                                                      :category (account-property
                                                                                  "The account that best matches the other side of the transaction"
                                                                                  category-accounts)}
                                                         :required ["date" "description" "amount" "category"]}}}
                           identify? (assoc :account (account-property
                                                       "The account the statement is for"
                                                       statement-accounts)))
             :required (cond-> ["period_start"
                                "period_end"
                                "opening_balance"
                                "closing_balance"
                                "transactions"]
                         identify? (conj "account"))}
      symbols add-investment-properties)))

(defn- investment-instructions
  [symbols]
  (cond-> [""
           "If this is the statement for an investment (brokerage or retirement) account,"
           "also extract the following."
           "- *opening_balance* and *closing_balance* are the cash balance of the account"
           "  only (e.g., \"Cash\" or \"Core Position\"), not its total value."
           "- *transactions* also include each trade. Give each line a *type*:"
           "  - *buy* A purchase of shares. The *amount* is the cash paid, so it is"
           "    negative."
           "  - *sell* A sale of shares. The *amount* is the cash received, so it is"
           "    positive."
           "  - *reinvest* A dividend used to buy more shares of the commodity that paid"
           "    it. Statements often show this as a dividend and a purchase; list it"
           "    once. The *amount* is the dividend, positive, and the *category* is the"
           "    income account the dividend belongs to."
           "  - *cash* Anything else: deposits, withdrawals, interest, fees, and"
           "    dividends paid in cash. The *category* is the other side, as above."
           "  For a *buy*, *sell* or *reinvest*, *symbol* is the ticker symbol of the"
           "  commodity and *shares* is the number of shares, always positive. For"
           "  *cash*, leave them null. The *category* of a *buy* or *sell* isn't used."
           "- *holdings* Each commodity held at the end of the period, with its ticker"
           "  *symbol* and the number of *shares*. Leave out cash and the core position."
           "If this isn't an investment statement, give every line the type *cash* and"
           "leave *holdings* empty."]
    (seq symbols) (conj ""
                        (str "The account holds or has held these symbols: "
                             (str/join ", " symbols)
                             "."))))

(defn prompt
  "Generates a prompt to read a statement and return structured data, given
  the entity and the account options (see account-options)"
  [{{:settings/keys [expense-hints]} :entity/settings}
   {:keys [symbols]}]
  (let [base [(if symbols
                "This is a bank account, credit card, or investment account statement. Extract the following."
                "This is a bank account or credit card statement. Extract the following.")
              "- *account* The enum value that best matches the account the statement is"
              "  for, judging by the institution and the kind of account (e.g., checking,"
              "  savings, or a credit card). If the statement covers more than one account,"
              "  read only the first."
              "- *institution* The name of the bank or card issuer."
              "- *period_start* and *period_end* The first and last days of the statement"
              "  period, written as YYYY-MM-DD."
              "- *opening_balance* The balance at the start of the period (e.g., the"
              "  \"Beginning Balance\" or \"Previous Balance\")."
              "- *closing_balance* The balance at the end of the period (e.g., the"
              "  \"Ending Balance\" or \"New Balance\")."
              "- *transactions* One entry for each transaction listed in the period."
              "  Leave out totals, summaries, and running balances."
              "  - *date* The date the transaction posted to the account, written as"
              "    YYYY-MM-DD. Statements often leave out the year; take it from the"
              "    statement period."
              "  - *description* The merchant or payee, without reference numbers."
              "  - *amount* The amount, signed by its effect on the balance: positive"
              "    when it increases the balance, negative when it decreases it. For a"
              "    bank account, deposits are positive and withdrawals are negative. For"
              "    a credit card, purchases are positive and payments and credits are"
              "    negative. Statements often print amounts without a sign and list them"
              "    under a heading that tells which they are."
              "  - *category* The enum value that best matches the other side of the"
              "    transaction: the expense for a purchase, the income for a deposit, or"
              "    the other account for a transfer or a payment."]]
    (str/join "\n"
              (cond-> base
                symbols (into (investment-instructions symbols))
                (seq expense-hints) (into (concat [""
                                                   "When selecting a category, follow these guidelines:"]
                                                  (map #(format "- %s" %) expense-hints)))))))

(defn- ->date
  [s]
  (when s
    (t/local-date (t/formatter "yyyy-MM-dd") s)))

(defn normalize
  "Parses the dates and amounts in the statement read by the model"
  [statement]
  (-> statement
      (update-in [:period-start] ->date)
      (update-in [:period-end] ->date)
      (update-in [:opening-balance] bigdec)
      (update-in [:closing-balance] bigdec)
      (update-in [:transactions]
                 (partial mapv #(cond-> (-> %
                                            (update-in [:date] ->date)
                                            (update-in [:amount] bigdec))
                                  (:shares %) (update-in [:shares] (comp abs bigdec)))))
      (cond-> (:holdings statement)
        (update-in [:holdings]
                   (partial mapv #(update-in % [:shares] bigdec))))))

; Banks post card purchases a few days after they're made, and a user may
; record a transaction on either date
(def match-window-days 7)

(defn candidate-items
  "Returns the items in the account that the statement lines may match:
  those in the statement period, or in the days before it, that aren't
  already part of a completed reconciliation."
  [account {:keys [period-start period-end]}]
  (let [items (ents/select
                (util/entity-type
                  {:transaction-item/account (util/->entity-ref account)
                   :transaction/transaction-date [:between
                                                  (t/minus period-start
                                                           (t/days match-window-days))
                                                  period-end]}
                  :transaction-item)
                {:select-also [:transaction/description
                               :transaction/transaction-date]})
        completed? (if-let [ids (->> items
                                     (keep (comp :id :transaction-item/reconciliation))
                                     set
                                     seq)]
                     (->> (ents/select {:id [:in ids]
                                        :reconciliation/status :completed})
                          (map :id)
                          set)
                     #{})]
    (->> items
         (remove (comp completed? :id :transaction-item/reconciliation))
         (map #(assoc %
                      :transaction-item/polarized-quantity
                      (polarize-quantity (:transaction-item/quantity %)
                                         (:transaction-item/action %)
                                         account))))))

(defn days-apart
  [d1 d2]
  (abs (t/time-between d1 d2 :days)))

(defn- amount-match?
  [{:keys [date amount]}
   {:transaction-item/keys [polarized-quantity]
    :transaction/keys [transaction-date]}]
  (and (zero? (compare amount polarized-quantity))
       (<= (days-apart date transaction-date)
           match-window-days)))

(defn match-items
  "Pairs each statement line with a transaction item having the same amount,
  dated within a few days of it. When more than one item fits, the nearest
  date wins, then the most similar description. Each item is used at most
  once. Returns a map with the :matched pairs of [line item] and the
  :unmatched lines.

  Options:
  :match? - a function of a line and an item that is true when the item may
            match the line, in place of the comparison of amount and date"
  [lines items & {:keys [match?] :or {match? amount-match?}}]
  (loop [lines (sort-by :date lines)
         items items
         result {:matched [] :unmatched []}]
    (if-let [line (first lines)]
      (if-let [item (->> items
                         (filter (partial match? line))
                         (sort-by (juxt #(days-apart (:date line)
                                                     (:transaction/transaction-date %))
                                        #(- (similarity (:description line)
                                                        (:transaction/description %)))))
                         first)]
        (recur (rest lines)
               (remove #(= (:id item) (:id %)) items)
               (update-in result [:matched] conj [line item]))
        (recur (rest lines)
               items
               (update-in result [:unmatched] conj line)))
      result)))

(defn- default-category
  [{:keys [amount]} account accounts-by-path]
  ; money coming into an asset account is most likely income, and money
  ; going out is most likely an expense
  (let [type (if (= (pos? amount)
                    (= :asset (:account/type account)))
               :income
               :expense)]
    (->> (vals accounts-by-path)
         (filter #(and (= type (:account/type %))
                       (leaf-account? %)))
         (sort-by ->path)
         first)))

(defn other-account
  "Returns the account for the other side of a statement line: the category
  chosen by the model or, if it chose none that fits, the most likely income
  or expense account"
  [{:keys [category] :as line} account accounts-by-path]
  (or (let [a (accounts-by-path category)]
        (if (and a (not= (:id a) (:id account)))
          a
          (default-category line account accounts-by-path)))
      (throw (ex-info "Unable to find an account for the other side of a statement line."
                      {:line line}))))

(defn make-trx
  "Returns a transaction, awaiting the user's review, for a statement line
  that matched nothing already recorded"
  [{:keys [date description amount] :as line}
   account
   entity
   accounts-by-path]
  (let [other (other-account line account accounts-by-path)
        quantity (abs amount)
        action (derive-action amount account)]
    #:transaction{:entity entity
                  :source :ingestion
                  :review-status :pending
                  :transaction-date date
                  :description description
                  :items [#:transaction-item{:account (util/->entity-ref account)
                                             :action action
                                             :quantity quantity}
                          #:transaction-item{:account (util/->entity-ref other)
                                             :action (if (= :debit action)
                                                       :credit
                                                       :debit)
                                             :quantity quantity}]}))

(defn reconciliation-in-progress
  "Returns the account's reconciliation in progress, if any"
  [account]
  (ents/find-by #:reconciliation{:account (util/->entity-ref account)
                                 :status :new}))

(defn last-completed-reconciliation
  "Returns the account's latest completed reconciliation, if any"
  [account]
  (ents/find-by #:reconciliation{:account (util/->entity-ref account)
                                 :status :completed}
                {:sort [[:reconciliation/end-of-period :desc]]}))

(defn release-items
  "Removes the items from the account's reconciliation in progress, if any,
  so that the items of a statement can take their place"
  [account]
  (when-let [recon (reconciliation-in-progress account)]
    (ents/put (assoc recon :reconciliation/items []))))

(defn make-reconciliation
  "Returns the reconciliation for the statement, containing the given items
  and referring to the statement's image. If the account already has a
  reconciliation in progress, the items are added to it, and the statement
  replaces any it had, as the latest statement has the closing balance."
  [account {:keys [period-end closing-balance]} items & {:keys [image]}]
  (let [existing (reconciliation-in-progress account)]
    (cond-> (merge existing
                   #:reconciliation{:account (util/->entity-ref account)
                                    :end-of-period period-end
                                    :balance closing-balance
                                    :status :new
                                    :items (->> items
                                                (concat (:reconciliation/items existing))
                                                (map :id)
                                                distinct
                                                (map (partial hash-map :id))
                                                trx-items/resolve-refs
                                                vec)})
      image (assoc :reconciliation/statement (util/->entity-ref image)))))
