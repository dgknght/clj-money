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
                                        polarize-quantity]]
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

(defn account-options
  "Returns the account paths the model may choose from for the account the
  statement is for, and for the other side of each transaction on it. When
  the account is already known, the model isn't asked to identify it."
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
    {:statement-accounts (when-not account
                           (->> accounts
                                (filter (comp statement-account-types
                                              :account/type))
                                (mapv ->path)))
     :category-accounts (->> accounts
                             (filter category?)
                             (mapv ->path))}))

(defn- account-property
  [description paths]
  (cond-> {:type "string"
           :description description}
    (seq paths) (assoc :enum paths)))

(defn schema
  "Returns a JSON schema describing the expected response, given the account
  paths the model may choose from."
  [{:keys [statement-accounts category-accounts]}]
  (let [identify? (seq statement-accounts)]
    {:type "object"
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
                 identify? (conj "account"))}))

(defn prompt
  "Generates a prompt to read a statement and return structured data"
  [{{:settings/keys [expense-hints]} :entity/settings}]
  (let [base ["This is a bank account or credit card statement. Extract the following."
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
              (if (seq expense-hints)
                (apply conj
                       base
                       ""
                       "When selecting a category, follow these guidelines:"
                       (map #(format "- %s" %) expense-hints))
                base))))

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
                 (partial mapv #(-> %
                                    (update-in [:date] ->date)
                                    (update-in [:amount] bigdec))))))

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

(defn- days-apart
  [d1 d2]
  (abs (t/time-between d1 d2 :days)))

(defn- match?
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
  :unmatched lines."
  [lines items]
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

(defn make-trx
  "Returns a transaction, awaiting the user's review, for a statement line
  that matched nothing already recorded"
  [{:keys [date description amount category] :as line}
   account
   entity
   accounts-by-path]
  (let [other (let [a (accounts-by-path category)]
                (if (and a (not= (:id a) (:id account)))
                  a
                  (default-category line account accounts-by-path)))
        quantity (abs amount)
        action (derive-action amount account)]
    (when-not other
      (throw (ex-info "Unable to find an account for the other side of a statement line."
                      {:line line})))
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

(defn make-reconciliation
  "Returns the reconciliation for the statement, containing the given items
  and referring to the statement's image. If the account already has a
  reconciliation in progress, the items are added to it, and the statement
  replaces any it had, as the latest statement has the closing balance."
  [account {:keys [period-end closing-balance]} items & {:keys [image]}]
  (let [existing (ents/find-by #:reconciliation{:account (util/->entity-ref account)
                                                :status :new})]
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
