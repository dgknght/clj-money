(ns clj-money.import.gnucash
  (:refer-clojure :exclude [update abs])
  (:require [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [clojure.set :refer [rename-keys]]
            [clojure.string :as s]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.core.async
             :refer [pipe
                     chan
                     close!
                     <!!
                     >!!]
             :as async]
            [clojure.data.xml :as xml]
            [config.core :refer [env]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [uuid
                                          update-in-if
                                          parse-int
                                          parse-bool]]
            [clj-money.util :refer [presence]]
            [clj-money.core]
            [clj-money.import :refer [read-source]])
  (:import [java.util.zip GZIPInputStream
            GZIPOutputStream]
           [java.io File FileInputStream
            FileOutputStream]
           [java.time Instant ZoneId]
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

(defn- parse-decimal
  [string-decimal]
  (when-let [match (re-find #"(-?\d+)\/(\d+)" string-decimal)]
    (with-precision 12
      (apply / (->> match
                    rest
                    (map bigdec))))))

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

(defmethod ^:private process-elem :default
  [state elem]
  (when (env :detailed-import-logging?)
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
        record (with-meta {:record-type record-type
                           :record-count (+ record-count
                                            (if (= :commodity record-type) 1 0))}
                          {:record-type :declaration})]
    (>!! out-chan record))
  (pop-elem state))

(defmethod ^:private process-elem ::gnc/commodity
  [{:keys [out-chan child-content] :as state} _]
  (>!! out-chan (with-meta (into {} (peek child-content))
                  {:record-type :commodity}))
  (pop-elem state))

(defmethod ^:private process-elem :price
  [{:keys [out-chan child-content] :as state} _]
  (>!! out-chan (with-meta (into {} (peek child-content))
                  {:record-type :price}))
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
      (>!! out-chan (with-meta account
                               {:record-type :account}))
      (when reconciliation
        (>!! out-chan (-> reconciliation
                          (assoc :account-id (:id account))
                          (with-meta {:record-type :reconciliation}))))))
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
  (>!! out-chan (with-meta (reduce (fn [r [k v]]
                                     (if (= :item k)
                                       (update-in r [:items] conj v)
                                       (if (blank-string? v)
                                         r
                                         (assoc r k v))))
                                   {:items []}
                                   (peek child-content))
                  {:record-type :budget}))
  (pop-elem state))

(defmethod ^:private process-elem ::bgt/slots
  [{:keys [child-content] :as state} _]
  (pop-elem state
            (peek child-content)))

(defn- match-tag-stack?
  [tag-stack & ks]
  (= ks
     (take-last (count ks) tag-stack)))

(defn- budget-period-stack?
  [tag-stack]
  (match-tag-stack? tag-stack
                    ::gnc/budget
                    ::bgt/slots
                    :slot
                    ::slot/value))

(defn- budget-item-stack?
  [tag-stack]
  (match-tag-stack? tag-stack
                    ::gnc/budget
                    ::bgt/slots
                    :slot))

(defn- reconciliation-stack?
  [tag-stack]
  (match-tag-stack? tag-stack
                    ::gnc/account
                    ::act/slots
                    :slot
                    ::slot/value))

(defn- reconcile-info-stack?
  [tag-stack]
  (match-tag-stack? tag-stack
                    ::gnc/account
                    ::act/slots
                    :slot))

(defn- template-quantity-stack?
  [tag-stack]
  (match-tag-stack? tag-stack
                    ::gnc/template-transactions
                    ::gnc/transaction
                    ::trn/splits
                    ::trn/split
                    ::split/slots
                    :slot
                    ::slot/value
                    :slot
                    ::slot/value))

(defn- template-detail-stack?
  [tag-stack]
  (match-tag-stack? tag-stack
                    ::gnc/template-transactions
                    ::gnc/transaction
                    ::trn/splits
                    ::trn/split
                    ::split/slots
                    :slot
                    ::slot/value))

(defn- template-details-stack?
  [tag-stack]
  (match-tag-stack? tag-stack
                    ::gnc/template-transactions
                    ::gnc/transaction
                    ::trn/splits
                    ::trn/split
                    ::split/slots
                    :slot))

(defn- stock-split-stack?
  [tag-stack]
  (match-tag-stack? tag-stack
                    ::gnc/transaction
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
                 [(with-meta (->> (peek child-content)
                                  (filter map?)
                                  (map (juxt (comp keyword :key) :value))
                                  (into {}))
                             {:record-type :reconciliation})]

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
  (>!! out-chan
       (with-meta
         (reduce aggregate-child-attr
                 {:splits []}
                 (peek child-content))
         {:record-type :transaction}))
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
                    (into {}))
        template (get-in templates [(:templ-acct record)])
        sched-tran (-> record
                       (dissoc :templ-acct)
                       (assoc :items (:items template))
                       (with-meta {:record-type :scheduled-transaction}))]
    (>!! out-chan sched-tran))
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

(defn- dispatch-record-type
  [record & _]
  (-> record meta :record-type))

(defmulti ^:private emit-record? dispatch-record-type)

(defmethod ^:private emit-record? :default
  [& _]
  true)

(defmethod ^:private emit-record? :declaration
  [record]
  (not= :book (:record-type record)))

(def ^:private filter-records
  (filter emit-record?))

(defmulti ^:private process-record dispatch-record-type)

(defmethod ^:private process-record :default
  [record _]
  record)

(defmethod ^:private process-record :commodity
  [{:keys [id name space quote_source] :as commodity} _]
  (let [result (-> commodity
                   (assoc :name (or name id)
                          :type (if (= "currency" quote_source)
                                  :currency
                                  (or (#{:fund :currency} (-> space s/lower-case keyword))
                                      :stock))
                          :symbol id)
                   (select-keys [:name :symbol :type :exchange])
                   (vary-meta assoc :ignore? (= "template" id)))
        exchange (#{:nasdaq :nyse :amex} (-> space s/lower-case keyword))]
    (if exchange
      (assoc result :exchange exchange)
      result)))

(defmethod ^:private process-record :price
  [price state]
  (let [trade-date (-> price :time :date parse-date)
        exchange (-> price :commodity :space s/lower-case keyword)
        symbol (-> price :commodity :id)
        k [:prices exchange symbol]]
    (swap! state assoc-in [k] trade-date)
    (-> price
        (assoc :trade-date trade-date
               :price (-> price :value parse-decimal)
               :exchange exchange
               :symbol symbol)
        (dissoc :time :value :commodity :currency))))

(defmethod ^:private process-record :account
  [account _]
  (let [account-type (account-types-map (:type account))]
    (when-not account-type
      (throw (ex-info (format "Unrecognized account type \"%s\"" (:type account))
                      {:account account})))
    (-> account
        (rename-keys {:parent :parent-id})
        (update-in [:type] account-types-map)
        (update-in [:commodity] #(-> %
                                     (rename-keys {:space :exchange
                                                   :id :symbol})
                                     (update-in [:exchange] (fn [e]
                                                              (when e
                                                                (if (= e "ISO4217")
                                                                  :currency
                                                                  (-> e
                                                                      s/lower-case
                                                                      keyword)))))))
        (vary-meta assoc :ignore? (contains? ignored-accounts (:name account))))))

(defn- seconds-to-date
  [seconds]
  (.atZone (Instant/ofEpochSecond seconds)
           (ZoneId/of "America/Chicago")))

(defmethod ^:private process-record :reconciliation
  [reconciliation _]
  (-> reconciliation
      (rename-keys {:last-date :end-of-period})
      (assoc :id (s/replace (str (uuid)) #"-" ""))
      (update-in-if [:include-children] parse-bool)
      (update-in-if [:end-of-period] (comp seconds-to-date
                                           parse-int))))

(defmulti ^:private refine-trading-transaction
  (fn [transaction]
    (let [actions (->> (:items transaction)
                       (map :action)
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
              [:items]
              (fn [items]
                (map (comp #(if (pred %)
                              (-> %
                                  (update-in [:quantity] abs)
                                  (update-in [:value] (fnil abs 0M)))
                              %)
                           #(select-keys % [:value ; TODO: This probably belongs somewhere more general
                                            :quantity
                                            :account-id
                                            :action
                                            :reconciled
                                            :memo]))
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
         (update-in item [:action] #(get-in trade-actions-map [%] %)))
       items))

(defmethod ^:private refine-trading-transaction :purchase
  [transaction]
  (let [commodity-item (->> (:items transaction)
                            (filter #(= :buy (:action %)))
                            first)]
    (-> transaction
        abs-items
        (assoc
         :action :buy
         :shares (:quantity commodity-item)
         :value (:value commodity-item)
         :commodity-account-id (:account-id commodity-item))
        (update-in [:items] adjust-trade-actions))))

(defmethod ^:private refine-trading-transaction :sell
  [transaction]
  (let [commodity-item (->> (:items transaction)
                            (filter #(= :sell (:action %)))
                            first)]
    (-> transaction
        abs-items
        (assoc
         :trade-date (:transaction-date transaction)
         :action :sell
         :shares (.abs (:quantity commodity-item))
         :commodity-account-id (:account-id commodity-item))
        (update-in [:items] adjust-trade-actions))))

(defmethod ^:private refine-trading-transaction :transfer
  [transaction]
  (let [from (->> (:items transaction)
                  (filter #(= :sell (:action %)))
                  first)
        to (->> (:items transaction)
                (filter #(= :buy (:action %)))
                first)]
    (-> transaction
        abs-items
        (assoc :action :transfer
               :shares (:quantity to)
               :value (:value to)
               :to-account-id (:account-id to)
               :from-account-id (:account-id from))
        (update-in [:items] adjust-trade-actions))))

(defmethod ^:private refine-trading-transaction :split
  [transaction]
  (let [split-item (->> (:items transaction)
                        (filter #(= :split (:action %)))
                        first)]
    (-> transaction
        (assoc :action :split
               :split-date (:transaction-date transaction)
               :shares-gained (:quantity split-item)
               :commodity-account-id (:account-id split-item))
        (update-in [:items] adjust-trade-actions)
        (abs-items #(not= :split (:action %))))))

(defn- process-transaction-item
  [item]
  (-> item
      (rename-keys {:account :account-id
                    :reconciled-state :reconciled})
      (update-in [:action] #(if (and %
                                     (#{"Debit" "Credit" "Buy" "Sell" "Split"} %))
                              (-> % s/lower-case keyword)
                              (if (= \- (first (:quantity item)))
                                :credit
                                :debit)))
      (update-in [:quantity] parse-decimal)
      (update-in [:value] parse-decimal)
      (update-in [:reconciled] #(= "y" %))))

(def ^:private ignore-transaction-patterns
  [#"(?i)^closing( year)? \d{4}"]) ; TODO: Maybe this should be entered in the UI?

(defmethod ^:private process-record :transaction
  [transaction _]
  (-> transaction
      (rename-keys {:splits :items
                    :date-posted :transaction-date})
      (update-in [:description] #(if ((some-fn nil? empty?) %)
                                   "*unspecified*"
                                   %))
      (update-in [:transaction-date] #(-> % :date parse-date))
      (update-in [:items] #(map process-transaction-item %))
      (vary-meta assoc
                 :ignore?
                 (boolean
                  (some #(re-find % (get-in transaction [:description] ""))
                        ignore-transaction-patterns)))
      refine-trading-transaction))

(defn- process-budget-item
  [item period-count]
  (let [periods (->> (:periods item)
                     (map (comp #(update-in % [1] parse-decimal)
                                #(update-in % [0] parse-int)
                                (juxt :key :value)))
                     (into {}))]
    (-> item
        (rename-keys {:key :account-id})
        (assoc :periods (map #(get-in periods [%] 0M)
                             (range period-count))))))

(defmethod ^:private process-record :budget
  [{:keys [num-periods] :as record} _]
  (let [period-count (parse-int num-periods)]
    (-> record
        (assoc :period (-> record :recurrence :period_type keyword)
               :start-date (-> record :recurrence :start :gdate parse-date)
               :period-count period-count)
        (update-in [:items] (fn [items]
                              (map #(process-budget-item % period-count)
                                   items)))
        (select-keys [:period :period-count :start-date :items :id :name]))))

(defn- parse-readable-number
  [value]
  (when (presence value)
    (-> value
        (s/replace "," "")
        bigdec)))

(defmethod ^:private process-record :scheduled-transaction
  [{:keys [schedule] :as record} _]
  (-> record
      (dissoc :schedule)
      (update-in [:start] parse-date)
      (update-in [:end] #(when % (parse-date %)))
      (update-in [:last] #(when % (parse-date %)))
      (update-in [:enabled] parse-bool)
      (update-in [:items]
                 (fn [items]
                   (map (fn [item]
                          (update-in-if item [:quantity] parse-readable-number))
                        items)))
      (rename-keys {:start :start-date
                    :end :end-date
                    :last :last-occurrence
                    :name :description})
      (assoc :interval-type (-> schedule
                                (get-in [:recurrence :period_type])
                                keyword)
             :interval-count (-> schedule
                                 (get-in [:recurrence :mult])
                                 parse-int))))

(defn- process-records
  [xf]
  (let [state (atom {})]
    (fn
      ([] (xf))
      ([acc] (xf acc))
      ([acc record]
       (xf acc (process-record record state))))))

(def ^:private reporting-types
  #{})

(defn- log-records
  [xf]
  (completing
   (fn [acc record]
     (let [record-type (-> record meta :record-type)]
       (when (reporting-types record-type)
         (log/debugf "reporting %s: %s"
                     (name record-type)
                     (prn-str record))))
     (xf acc record))))

(defmethod read-source :gnucash
  [_ inputs out-chan]
  (let [records-chan (chan 100 (comp filter-records
                                     process-records
                                     log-records))
        _ (pipe records-chan out-chan)]
    (->> inputs
         (map #(GZIPInputStream. %))
         (map io/reader)
         (mapcat #(xml/event-seq % {}))
         (reduce process-event
                 {:elems []
                  :content []
                  :child-content []
                  :out-chan records-chan}))
    (close! records-chan)))

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
  (let [records-chan (chan)
        state (chunk-file-state args)
        result (async/reduce #(-> %1
                                  (update-in [:records] conj %2)
                                  evaluate-output)
                             state
                             records-chan)]
    (println "reading the source file...")
    (with-open [input-stream (FileInputStream. (:input-file state))]
      (read-source :gnucash [input-stream] records-chan))
    (process-output (<!! result))
    (println "\nDone")))
