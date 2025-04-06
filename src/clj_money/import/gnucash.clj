(ns clj-money.import.gnucash
  (:refer-clojure :exclude [update abs])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.core.async :as a]
            [clojure.data.xml :as xml]
            [config.core :refer [env]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [uuid
                                          parse-int
                                          parse-bool]]
            [clj-money.dates :as dates]
            [clj-money.util :as util :refer [presence]]
            [clj-money.core]
            [clj-money.import :refer [read-source]])
  (:import [java.util.zip GZIPInputStream
            GZIPOutputStream]
           [java.io
            File
            FileInputStream
            FileOutputStream]
           java.math.BigDecimal
           [clojure.data.xml.event StartElementEvent
            CharsEvent
            EndElementEvent]))

(defn- blank-string?
  [v]
  (and (string? v)
       (s/blank? v)))

(defmulti ^:private parse-date
  #(if (map? %)
     :elem
     :string))

(defmethod ^:private parse-date :elem
  [{:keys [gdate]}]
  (parse-date gdate))

(defmethod ^:private  parse-date :string
  [string-date]
  (when-let [match (re-find #"^(\d{4})-(\d{2})-(\d{2})" string-date)]
    (apply t/local-date (->> match
                             rest
                             (map parse-int)))))

(defn- round-decimal
  ([places]
   #(round-decimal % places))
  ([^BigDecimal d places]
   (.setScale d places BigDecimal/ROUND_HALF_UP)))

(defn- parse-decimal
  [string-decimal]
  (when-let [match (re-find #"(-?\d+)\/(\d+)" string-decimal)]
    (with-precision 19
      (round-decimal
        (apply / (->> match
                      rest
                      (map bigdec)))
        6))))

(def ^:private account-types-map
  {"ASSET"      :asset
   "BANK"       :asset
   "INCOME"     :income
   "EXPENSE"    :expense
   "LIABILITY"  :liability
   "EQUITY"     :equity
   "CREDIT"     :liability
   "STOCK"      :asset
   "MUTUAL"     :asset
   "CASH"       :asset
   "PAYABLE"    :liability
   "RECEIVABLE" :asset
   "ROOT"       :root})

(def ^:private ignored-accounts #{"Root Account" "Assets" "Liabilities" "Equity" "Income" "Expenses"})

(xml/alias-uri :gnc          "http://www.gnucash.org/XML/gnc"
               :book         "http://www.gnucash.org/XML/book"
               :slot         "http://www.gnucash.org/XML/slot"
               :cd           "http://www.gnucash.org/XML/cd"
               :act          "http://www.gnucash.org/XML/act"
               :cmdty        "http://www.gnucash.org/XML/cmdty"
               :price        "http://www.gnucash.org/XML/price"
               :bgt          "http://www.gnucash.org/XML/bgt"
               :trn          "http://www.gnucash.org/XML/trn"
               :ts           "http://www.gnucash.org/XML/ts"
               :split        "http://www.gnucash.org/XML/split"
               :schedxaction "http://www.gnucash.org/XML/schedxaction"
               :sx           "http://www.gnucash.org/XML/sx"
               :rec          "http://www.gnucash.org/XML/recurrence")

(defn- agg-text-content
  [values]
  (s/trim (s/join "" values)))

(def ^:private attribute-elements
  #{::act/name
    ::act/id
    ::act/parent
    ::act/type
    ::cmdty/space
    ::cmdty/id
    ::cmdty/name
    ::cmdty/xcode
    ::cmdty/quote_source
    ::price/value
    ::bgt/id
    ::bgt/name
    ::bgt/description
    ::bgt/num-periods
    ::rec/mult
    ::rec/period_type
    ::slot/key
    :gdate
    ::trn/description
    ::ts/date
    ::split/reconciled-state
    ::split/memo
    ::split/quantity
    ::split/value
    ::split/account
    ::split/action
    ::sx/name
    ::sx/enabled
    ::sx/templ-acct})

(def ^:private compound-attribute-elements
  #{::bgt/recurrence
    ::rec/start
    ::price/commodity
    ::price/currency
    ::price/time
    ::trn/date-posted
    ::sx/schedule
    ::sx/start
    ::sx/end
    ::sx/last
    ::gnc/recurrence})

(defn- push-child-content
  [existing-content new-content]
  (->> new-content
       (filter seq)
       (reduce (fn [all-content new-part]
                 (conj (pop all-content)
                       (conj (peek all-content) new-part)))
               existing-content)))

(defn- pop-elem
  ([state] (pop-elem state nil))
  ([state child-content]
   (-> state
       (update-in [:elems] pop)
       (update-in [:content] pop)
       (update-in [:child-content] pop)
       (update-in [:child-content] push-child-content child-content))))

(defmulti ^:private process-elem
  (fn [_ {:keys [tag]}]
    (cond
      (attribute-elements tag)          :attribute
      (compound-attribute-elements tag) :compound-attribute
      :else                             tag)))

(def ^:private known-irrelevant-elements
  #{::book/id
    ::cmdty/scu
    ::gnc/book
    :gnc-v2
    ::price/id
    ::split/id
    ::trn/id
    ::trn/currency
    ::trn/date-entered
    ::trn/slots})

(defmethod ^:private process-elem :default
  [state elem]
  (when (and (env :detailed-import-logging?)
             (not (known-irrelevant-elements (:tag elem))))
    (log/debug "Encountered unhandled element " (prn-str (:tag elem))))
  (pop-elem state))

(defn- tag->keyword
  [tag]
  (keyword (last (s/split (name tag) #"\/"))))

; For simple elements that represent attributes of their
; containing elemen, return a name/value pair that will
; later be turned into a map representing the record
(defmethod ^:private process-elem :attribute
  [{:keys [content] :as state} {tag :tag}]
  (pop-elem state
            [[(tag->keyword tag)
              (agg-text-content (peek content))]]))

(defmethod ^:private process-elem :compound-attribute
  [{:keys [child-content] :as state} {:keys [tag]}]
  (pop-elem state
            [[(tag->keyword tag)
              (into {} (peek child-content))]]))

(def ^:private record-type-subs
  {:schedxaction :scheduled-transaction})

(defmethod ^:private process-elem ::gnc/count-data
  [{:keys [out-chan content] :as state} {:keys [attrs]}]
  (let [raw-type (keyword (::cd/type attrs))
        record-type (get-in record-type-subs [raw-type] raw-type)
        record-count (parse-int (agg-text-content (peek content)))
        record {:import/record-type :declaration
                :declaration/record-type record-type
                :declaration/record-count record-count}]
    (a/>!! out-chan record))
  (pop-elem state))

(defmethod ^:private process-elem ::gnc/commodity
  [{:keys [out-chan child-content] :as state} _]
  (a/>!! out-chan (into {:import/record-type :commodity}
                      (peek child-content)))
  (pop-elem state))

(defmethod ^:private process-elem :price
  [{:keys [out-chan child-content] :as state} _]
  (a/>!! out-chan (into {:import/record-type :price}
                      (peek child-content)))
  (pop-elem state))

(defn- template?
  [elems]
  (let [tag-set (set (map :tag elems))]
    (tag-set ::gnc/template-transactions)))

(defmethod ^:private process-elem ::gnc/account
  [{:keys [out-chan child-content elems] :as state} _]
  (when-not (template? elems)
    (let [account (->> (peek child-content)
                       (remove map?)
                       (into {}))
          reconciliation (->> (peek child-content)
                              (filter map?)
                              (remove #(#{"placeholder"
                                          "color"
                                          "code"
                                          "last-num"
                                          "notes"
                                          "hidden"
                                          "copy-number"
                                          "tax-related"
                                          "payer-name-source"} (:key %))) ; TODO: probably should catch this earlier in the process
                              first)]
      (a/>!! out-chan (assoc account
                           :import/record-type :account))
      (when reconciliation
        (a/>!! out-chan (assoc reconciliation
                             :account-id (:id account)
                             :import/record-type :reconciliation)))))
  (pop-elem state))

(defmethod ^:private process-elem ::act/slots
  [{:keys [child-content] :as state} _]
  (pop-elem state (peek child-content)))

(defmethod ^:private process-elem ::act/commodity
  [{:keys [child-content] :as state} _]
  (pop-elem state
            [[:commodity (into {} (peek child-content))]]))

(defmethod ^:private process-elem ::gnc/budget
  [{:keys [out-chan child-content] :as state} _]
  (a/>!! out-chan (reduce (fn [r [k v]]
                          (if (= :item k)
                            (update-in r [:budget/items] conj v)
                            (if (blank-string? v)
                              r
                              (assoc r k v))))
                        {:budget/items []
                         :import/record-type :budget}
                        (peek child-content)))
  (pop-elem state))

(defmethod ^:private process-elem ::bgt/slots
  [{:keys [child-content] :as state} _]
  (pop-elem state
            (peek child-content)))

(defn- match-tag-stack?
  [& ks]
  (fn [tag-stack]
    (= ks
       (take-last (count ks) tag-stack))))

(def ^:private budget-period-stack?
  (match-tag-stack? ::gnc/budget
                    ::bgt/slots
                    :slot
                    ::slot/value))

(def ^:private budget-item-stack?
  (match-tag-stack? ::gnc/budget
                    ::bgt/slots
                    :slot))

(def ^:private reconciliation-stack?
  (match-tag-stack? ::gnc/account
                    ::act/slots
                    :slot
                    ::slot/value))

(def ^:private reconcile-info-stack?
  (match-tag-stack? ::gnc/account
                    ::act/slots
                    :slot))

(def ^:private template-quantity-stack?
  (match-tag-stack? ::gnc/template-transactions
                    ::gnc/transaction
                    ::trn/splits
                    ::trn/split
                    ::split/slots
                    :slot
                    ::slot/value
                    :slot
                    ::slot/value))

(def ^:private template-detail-stack?
  (match-tag-stack? ::gnc/template-transactions
                    ::gnc/transaction
                    ::trn/splits
                    ::trn/split
                    ::split/slots
                    :slot
                    ::slot/value))

(def ^:private template-details-stack?
  (match-tag-stack? ::gnc/template-transactions
                    ::gnc/transaction
                    ::trn/splits
                    ::trn/split
                    ::split/slots
                    :slot))

(def ^:private stock-split-stack?
  (match-tag-stack? ::gnc/transaction
                    ::trn/splits
                    ::trn/split
                    ::split/slots
                    :slot
                    ::slot/value))

(defmethod ^:private process-elem ::slot/value
  [{:keys [content child-content elems] :as state} {:keys [tag]}]
  (let [tag-stack (map :tag elems)
        result (cond
                 (budget-period-stack? tag-stack) [[:periods (peek child-content)]]
                 (reconciliation-stack? tag-stack) (peek child-content)
                 (template-quantity-stack? tag-stack) [[:value (->> content
                                                                    flatten
                                                                    (remove empty?)
                                                                    (s/join ""))]]
                 (template-detail-stack? tag-stack) (let [m (->> (peek child-content)
                                                                 (map (juxt (comp keyword :key)
                                                                            :value))
                                                                 (into {}))]
                                                      (if (seq (:credit-formula m))
                                                        [[:action :credit]
                                                         [:quantity (:credit-formula m)]
                                                         [:account-id (:account m)]]
                                                        [[:action :debit]
                                                         [:quantity (:debit-formula m)]
                                                         [:account-id (:account m)]]))

                 (stock-split-stack? tag-stack) (log/warnf "ignoring value \"%s\" at key \"%s\" because it is believed to be insigificant"
                                                           (agg-text-content (peek content))
                                                           (tag->keyword tag))
                 :else
                 [[(tag->keyword tag)
                   (agg-text-content (peek content))]])]
    (pop-elem state result)))

(defmethod ^:private process-elem ::split/slots
  [{:keys [child-content] :as state} _]
  (pop-elem state (peek child-content)))

(defn- reconcile-info-content?
  [child-content]
  (= "reconcile-info"
     (-> child-content
         peek
         first
         second)))

; slot elements are used as generic key-value pair containers
; in the gnucash XML. It's necessary to inspect the parent
; chain in order to know how to handle the content
(defmethod ^:private process-elem :slot
  [{:keys [elems child-content] :as state} _]
  (let [tag-stack (map :tag elems)
        result (cond
                 (budget-item-stack? tag-stack)
                 [[:item (into {} (peek child-content))]]

                 (and (reconcile-info-stack? tag-stack)
                      (reconcile-info-content? child-content))
                 [(->> (peek child-content)
                       (filter map?)
                       (map (juxt (comp keyword :key) :value))
                       (into {:import/record-type :reconciliation}))]

                 (template-details-stack? tag-stack)
                 (peek child-content)

                 :else [(into {} (peek child-content))])]
    (pop-elem state result)))

(defn- process-template-elem
  [{:keys [child-content] :as state}]
  (let [content (->> (peek child-content)
                     (group-by first)
                     (map (fn [entry]
                            (update-in entry [1] #(map second %))))
                     (into {}))
        template {:id (:account (first (:split content)))
                  :description (first (:description content))
                  :items (map #(select-keys % [:action
                                               :quantity
                                               :account-id])
                              (:split content))}]

    (-> state
        (update-in [:templates] (fnil assoc {}) (:id template) template)
        pop-elem)))

(defn- aggregate-child-attr
  [result [k v]]
  (if (= :split k)
    (update-in result [:splits] conj v)
    (if (blank-string? v)
      result
      (do
        (when-let [existing (get-in result [k])]
          (log/warnf "Replacing existing value \"%s\" with \"%s\" at key \"%s\" in transaction"
                     existing
                     v
                     k))
        (assoc result k v)))))

(defn- process-transaction-elem
  [{:keys [out-chan child-content] :as state}]
  (a/>!! out-chan
         (reduce aggregate-child-attr
                 {:splits []
                  :import/record-type :transaction}
                 (peek child-content)))
  (pop-elem state))

(defmethod ^:private process-elem ::gnc/transaction
  [{:keys [elems] :as state} _]
  (if (template? elems)
    (process-template-elem state)
    (process-transaction-elem state)))

(defmethod ^:private process-elem ::trn/splits
  [{:keys [child-content] :as state} _]
  (pop-elem state (peek child-content)))

(defmethod ^:private process-elem ::trn/split
  [{:keys [child-content] :as state} _]
  (pop-elem state [[:split (into {} (peek child-content))]]))

(defmethod ^:private process-elem ::gnc/schedxaction
  [{:keys [out-chan child-content templates] :as state} _]
  (let [record (->> child-content
                    flatten
                    (partition 2)
                    (map vec)
                    (into {:import/record-type :scheduled-transaction}))
        template (get-in templates [(:templ-acct record)])
        sched-tran (-> record
                       (dissoc :templ-acct)
                       (assoc :items (:items template)))]
    (a/>!! out-chan sched-tran))
  (pop-elem state))

(defmulti ^:private process-event
  (fn [_ event]
    (type event)))

(defmethod ^:private process-event :default
  [_ event]
  (log/warn "Unhandled event type " (type event)))

(defmethod ^:private process-event StartElementEvent
  [state event]
  (-> state
      (update-in [:elems] conj event)
      (update-in [:content] conj [])
      (update-in [:child-content] conj [])))

(defmethod ^:private process-event CharsEvent
  [state event]
  (if (s/blank? (:str event))
    state
    (update-in state [:content] #(conj (pop %)
                                       (conj (peek %) (:str event))))))

(defmethod ^:private process-event EndElementEvent
  [{:keys [elems] :as state} _]
  (process-elem state (peek elems)))

(defmulti ^:private emit-record? :import/record-type)

(defmethod ^:private emit-record? :default
  [& _]
  true)

(defmethod ^:private emit-record? :declaration
  [record]
  (not= :book (:declaration/record-type record)))

(defmulti ^:private process-record (fn [r & _]
                                     (:import/record-type r)))

(defmethod ^:private process-record :default
  [record _]
  record)

(defmethod ^:private process-record :commodity
  [{:keys [id name quote_source] :as record} _]
  (let [space (-> record :space s/lower-case keyword)
        exchange (#{:nasdaq :nyse :amex} space)
        type (if (or (#{:crypto} space) ; TODO This is a custom namespace I created. we probably need to be able to map this in the UI
                     (#{"currency"} quote_source))
               :currency
               (or (#{:fund :currency} space)
                   :stock))]
    (cond-> {:import/record-type :commodity
             :import/ignore? (= "template" id)
             :commodity/name (or name id)
             :commodity/symbol id
             :commodity/type type}
      exchange (assoc :commodity/exchange exchange))))

(defmethod ^:private process-record :price
  [price state]
  (let [trade-date (-> price :time :date parse-date)
        exchange (-> price :commodity :space s/lower-case keyword)
        symbol (-> price :commodity :id)
        k [:prices exchange symbol]]
    (swap! state assoc-in [k] trade-date)
    {:import/record-type :price
     :import/ignore? false
     :price/trade-date trade-date
     :price/price (-> price :value parse-decimal)
     :commodity/symbol symbol
     :commodity/exchange exchange}))

(def ^:private currency?
  #{:iso4217 :currency})

(defn- account-commodity-type
  [{:keys [commodity]}]
  (let [space (some-> (:space commodity)
                      s/lower-case
                      keyword)]
    {:type (if (currency? space)
             :currency
             :stock) ; TODO: Can we distinguish a stock from a fund here?
     :exchange (when (not (currency? space))
                 space)}))

(defmethod ^:private process-record :account
  [{:as record :keys [commodity]} _]
  (if-let [account-type (account-types-map (:type record))]
    (cond-> {:import/record-type :account
             :import/id (:id record)
             :import/parent-id (:parent record)
             :import/ignore? (contains? ignored-accounts (:name record))
             :account/type account-type
             :account/name (:name record)}
      commodity (assoc :import/commodity
                       (let [{:keys [type exchange]} (account-commodity-type record)]
                         (cond-> {:commodity/symbol (-> record :commodity :id)
                                  :commodity/type type}
                           exchange
                           (assoc :commodity/exchange exchange)))))
    (throw (ex-info (format "Unrecognized account type \"%s\"" (:type record))
                    {:account record}))))

(defn- parse-reconciliation-date
  [d]
  (some-> d
          parse-int
          dates/of-epoch-second
          (dates/at-zone "America/Chicago")
          t/local-date))

(defmethod ^:private process-record :reconciliation
  [record _]
  (let [end-of-period (parse-reconciliation-date (:last-date record))]
    (if end-of-period
      {:import/record-type :reconciliation
       :import/id (s/replace (str (uuid)) #"-" "")
       :import/include-children? (parse-bool (:include-children record))
       :import/account-id (:account-id record)
       :reconciliation/end-of-period end-of-period}
      (log/warnf "[import] Unable to parse reconciliation date: %s" record))))

(defmulti ^:private refine-trading-transaction
  (fn [transaction]
    (let [actions (->> (:transaction/items transaction)
                       (map :transaction-item/action)
                       set)]
      (cond
        (empty? actions)              :none
        (= #{:debit :credit} actions) :none
        (= #{:buy :sell} actions)     :transfer
        (:sell actions)               :sell
        (:buy actions)                :purchase
        (:split actions)              :split))))

(defn- abs
  [^BigDecimal value]
  (.abs value))

(defn- abs-items
  ([transaction]
   (abs-items transaction (constantly true)))
  ([transaction pred]
   (update-in transaction
              [:transaction/items]
              (fn [items]
                (map #(if (pred %)
                        (-> %
                            (update-in [:transaction-item/quantity] abs)
                            (update-in [:transaction-item/value] (fnil abs 0M)))
                        %)
                     items)))))

(defmethod ^:private refine-trading-transaction :default
  [transaction]
  (abs-items transaction))

(def ^:private trade-actions-map
  {:buy :debit
   :sell :credit
   :split :debit})

(defn- adjust-trade-actions
  [items]
  (map (fn [item]
         (update-in item [:transaction-item/action] #(get-in trade-actions-map [%] %)))
       items))

(defn- trx-item-by-action
  [{:transaction/keys [items]} action]
  (->> items
       (filter #(= action (:transaction-item/action %)))
       first))

(defmethod ^:private refine-trading-transaction :purchase
  [transaction]
  (let [commodity-item (trx-item-by-action transaction :buy)]
    (-> transaction
        abs-items
        (assoc
         :trade/action :buy
         :trade/shares (:transaction-item/quantity commodity-item)
         :trade/value (:transaction-item/value commodity-item)
         :import/commodity-account-id (:import/account-id commodity-item))
        (update-in [:transaction/items] adjust-trade-actions))))

(defmethod ^:private refine-trading-transaction :sell
  [transaction]
  (let [commodity-item (trx-item-by-action transaction :sell)]
    (-> transaction
        abs-items
        (assoc
          :trade/date (:transaction/transaction-date transaction)
          :trade/action :sell
          :trade/value (abs (:transaction-item/value commodity-item))
          :trade/shares (abs (:transaction-item/quantity commodity-item))
          :import/commodity-account-id (:import/account-id commodity-item))
        (update-in [:transaction/items] adjust-trade-actions))))

(defmethod ^:private refine-trading-transaction :transfer
  [transaction]
  (let [from (trx-item-by-action transaction :sell)
        to (trx-item-by-action transaction :buy)]
    (-> transaction
        abs-items
        (assoc :trade/action :transfer
               :transfer/shares (:transaction-item/quantity to)
               :transfer/value (:transaction-item/value to)
               :import/to-account-id (:import/account-id to)
               :import/from-account-id (:import/account-id from))
        (update-in [:transaction/items] adjust-trade-actions))))

(defmethod ^:private refine-trading-transaction :split
  [transaction]
  (let [split-item (trx-item-by-action transaction :split)]
    (-> transaction
        (assoc :trade/action :split
               :split/date (:transaction/transaction-date transaction)
               :split/shares-gained (:transaction-item/quantity split-item)
               :import/commodity-account-id (:import/account-id split-item))
        (update-in [:transaction/items] adjust-trade-actions)
        (abs-items #(not= :split (:transaction-item/action %))))))

(defn- translate-action
  [{:keys [action quantity]}]
  (if (and action
           (#{"Debit" "Credit" "Buy" "Sell" "Split"} action))
    (-> action s/lower-case keyword)
    (if (= \- (first quantity))
      :credit
      :debit)))

(defn- process-transaction-item
  [item]
  (cond-> {:transaction-item/quantity (-> item :quantity parse-decimal)
           :transaction-item/value (-> item :value parse-decimal)
           :transaction-item/action (translate-action item)
           :import/account-id (:account item)
           :import/reconciled? (= "y" (:reconciled-state item))}
    (:memo item) (assoc :transaction-item/memo (:memo item))))

(defn- re-find-any
  [& patterns]
  (fn [v]
    (when v
      (boolean
        (some (fn [p]
                (re-find p v))
              patterns)))))

(def ^:private ignore-trx?
  (re-find-any #"(?i)^closing( year)? \d{4}"))

(defmethod ^:private process-record :transaction
  [record _]
  (refine-trading-transaction
    {:import/record-type :transaction
     :import/ignore? (ignore-trx? (:description record))
     :transaction/description (util/presence-or (:description record)
                                                "*unspecified*")
     :transaction/items (map process-transaction-item
                             (:splits record))
     :transaction/transaction-date (-> record
                                       :date-posted
                                       :date
                                       parse-date)}))

(defn- process-budget-item
  [item period-count]
  (let [periods (->> (:periods item)
                     (map (comp #(update-in % [1] parse-decimal)
                                #(update-in % [0] parse-int)
                                (juxt :key :value)))
                     (into {}))]
    {:budget-item/periods (map #(get-in periods [%] 0M)
                             (range period-count))
     :import/account-id (:key item)}))

(defmethod ^:private process-record :budget
  [{:keys [num-periods] :as record} _]
  (let [period-count (parse-int num-periods)]
    {:import/id (:id record)
     :import/record-type :budget
     :budget/name (:name record)
     :budget/period (-> record :recurrence :period_type keyword)
     :budget/start-date (-> record :recurrence :start :gdate parse-date)
     :budget/period-count period-count
     :budget/items (map #(process-budget-item % period-count)
                        (:budget/items record))}))

(defn- parse-readable-number
  [value]
  (when (presence value)
    (-> value
        (s/replace "," "")
        bigdec)))

(defn- ->scheduled-transaction-item
  [{:keys [quantity action account-id]}]
  {:scheduled-transaction-item/quantity (parse-readable-number quantity)
   :scheduled-transaction-item/action action
   :import/account-id account-id})

(defmethod ^:private process-record :scheduled-transaction
  [{:keys [schedule] :as record} _]
  {:scheduled-transaction/start-date (parse-date (:start record))
   :scheduled-transaction/end-date (when-let [d (:end record)]
                                     (parse-date d))
   :scheduled-transaction/enabled (parse-bool (:enabled record))
   :scheduled-transaction/last-occurrence (when-let [d (:last record)]
                                            (parse-date d))
   :scheduled-transaction/description (:name record)
   :scheduled-transaction/items (map ->scheduled-transaction-item
                                     (:items record))
   :scheduled-transaction/interval-type (-> schedule
                                            (get-in [:recurrence :period_type])
                                            keyword)
   :scheduled-transaction/interval-count (-> schedule
                                             (get-in [:recurrence :mult])
                                             parse-int)
   :import/record-type :scheduled-transaction})

(defn- process-records []
  (let [state (atom {})]
    (map #(process-record % state))))

(defn- log-records
  [xf]
  (completing
    (fn [acc {:import/keys [record-type] :as record}]
      (log/debugf "[import] [gnucash] reporting %s: %s"
                  (name record-type)
                  (prn-str record))
      (xf acc record))))

(defmethod read-source :gnucash
  [_ inputs]
  (let [out-chan (a/chan)
        records-chan (a/chan 100 (comp (filter emit-record?)
                                       (process-records)
                                       (filter identity)
                                       log-records))]
    (a/pipe records-chan out-chan)
    (a/go
      (->> inputs
           (map #(GZIPInputStream. %))
           (map io/reader)
           (mapcat #(xml/event-seq % {}))
           (reduce process-event
                   {:elems []
                    :content []
                    :child-content []
                    :out-chan records-chan}))
      (a/close! records-chan))
    out-chan))

(defn- process-output
  "Writes the records to the next output file and resets the record buffer"
  [{:keys [records
           output-folder
           file-name
           output-paths]
    :as context}]
  (if (seq records)
    (let [output-path (format "%s/%s_%s.edn.gz"
                              output-folder
                              file-name
                              (count output-paths))]
      (println "compressing and writing records...")
      (binding [*print-meta* true]
        (with-open [writer (io/writer (GZIPOutputStream. (FileOutputStream. output-path)))]
          (with-precision 4
            (.write writer (prn-str records)))))
      (println output-path)
      (-> context
          (assoc :records [])
          (update-in [:output-paths] #(conj % output-path))))
    context))

(def ^:private chunk-file-options
  [["-m" "--maximum MAXIMUM" "The maximum number of records to include in each output file"
    :parse-fn parse-int
    :default 10000]])

(defn- parse-chunk-file-opts
  "Accepts the raw arguments passed into chunk-file
  and returns a map containing the information needed
  to service the request"
  [args]
  (let [opts (parse-opts args chunk-file-options)
        input-path (-> opts :arguments first)
        input-file (File. input-path)
        output-folder (.getParent input-file)
        file-name (.getName input-file)]
    {:options (:options opts)
     :input-file input-file
     :file-name file-name
     :output-folder output-folder}))

(defn- evaluate-output
  "Evaluates the output for the chunking process
  and writes the file and resets the record buffer if the
  maximum number of records has been reached"
  ([context _] (evaluate-output context))
  ([{:keys [records max-record-count] :as context}]
   (if (>= (count records) max-record-count)
     (process-output context)
     context)))

(defn- chunk-file-state
  [args]
  (let [{:keys [input-file
                output-folder
                file-name]
         {:keys [maximum]} :options} (parse-chunk-file-opts args)]
    {:output-paths []
     :input-file input-file
     :records []
     :max-record-count maximum
     :file-name (second (re-matches #"^(.+)(\..+)$" file-name))
     :output-folder output-folder}))

(defn chunk-file
  "Accepts a path to a gnucash file and creates multiple, smaller files
  that container the same data, returning the paths to the new files"
  [& args]
  (let [records-chan (a/chan)
        state (chunk-file-state args)
        result (a/reduce #(-> %1
                              (update-in [:records] conj %2)
                              evaluate-output)
                         state
                         records-chan)]
    (println "reading the source file...")
    (with-open [input-stream (FileInputStream. (:input-file state))]
      (read-source :gnucash [input-stream] records-chan))
    (process-output (a/<!! result))
    (println "\nDone")))
