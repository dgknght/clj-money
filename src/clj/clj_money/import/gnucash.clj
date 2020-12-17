(ns clj-money.import.gnucash
  (:refer-clojure :exclude [update])
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
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-time.coerce :as tc]
            [clj-money.util :refer [uuid
                                    update-in-if
                                    parse-bool]]
            [clj-money.core]
            [clj-money.import :refer [read-source]])
  (:import [java.util.zip GZIPInputStream
                          GZIPOutputStream]
           [java.io File FileInputStream
                         FileOutputStream]
           [clojure.data.xml.event StartElementEvent
                                   CharsEvent
                                   EndElementEvent]))

(defn- blank-string?
  [v]
  (and (string? v)
       (s/blank? v)))

(defn- parse-integer
  [string-integer]
  (Integer/parseInt string-integer))

(defn- parse-date
  [string-date]
  (when-let [match (re-find #"^(\d{4})-(\d{2})-(\d{2})" string-date)]
    (apply t/local-date (->> match
                             rest
                             (map parse-integer)))))

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

(xml/alias-uri :gnc   "http://www.gnucash.org/XML/gnc"
               :slot  "http://www.gnucash.org/XML/slot"
               :cd    "http://www.gnucash.org/XML/cd"
               :act   "http://www.gnucash.org/XML/act"
               :cmdty "http://www.gnucash.org/XML/cmdty"
               :price "http://www.gnucash.org/XML/price"
               :bgt   "http://www.gnucash.org/XML/bgt"
               :trn   "http://www.gnucash.org/XML/trn"
               :ts    "http://www.gnucash.org/XML/ts"
               :split "http://www.gnucash.org/XML/split"
               :rec   "http://www.gnucash.org/XML/recurrence")

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
    ::split/action})

(def ^:private compound-attribute-elements
  #{::bgt/recurrence
    ::rec/start
    ::price/commodity
    ::price/currency
    ::price/time
    ::trn/date-posted})

(defmulti ^:private process-elem
  (fn [_ {:keys [tag]}]
    (cond
      (attribute-elements tag)          :attribute
      (compound-attribute-elements tag) :compound-attribute
      :else                             tag)))

(defmethod ^:private process-elem :default
  [_ elem]
  (when (env :detailed-import-logging?)
    (log/debug "Encountered unhandled element " (prn-str (:tag elem))))
  nil)

(defn- tag->keyword
  [tag]
  (keyword (last (s/split (name tag) #"\/"))))

; For simple elements that represent attributes of their
; containing elemen, return a name/value pair that will
; later be turned into a map representing the record
(defmethod ^:private process-elem :attribute
  [{:keys [content]} {tag :tag}]
  [[(tag->keyword tag)
    (agg-text-content (peek content))]])

(defmethod ^:private process-elem :compound-attribute
  [{:keys [child-content]} {:keys [tag]}]
  [[(tag->keyword tag)
    (into {} (peek child-content))]])

(defmethod ^:private process-elem ::gnc/count-data
  [{:keys [out-chan content]} {:keys [attrs]}]
  (let [record-type (keyword (::cd/type attrs))
        record-count (parse-integer (agg-text-content (peek content)))
        record (-> {:record-type record-type
                    :record-count (+ record-count
                                     (if (= :commodity record-type) 1 0))}
                   (with-meta {:record-type :declaration}))]
    (>!! out-chan record)
    nil))

(defmethod ^:private process-elem ::gnc/commodity
  [{:keys [out-chan child-content]} _]
  (>!! out-chan (with-meta (into {} (peek child-content))
                           {:record-type :commodity}))
  nil)

(defmethod ^:private process-elem :price
  [{:keys [out-chan child-content]} _]
  (>!! out-chan (with-meta (into {} (peek child-content))
                           {:record-type :price}))
  nil)

(defmethod ^:private process-elem ::gnc/account
  [{:keys [out-chan child-content]} _]
  (let [account (->> (peek child-content)
                     (remove map?)
                     (into {}))
        reconciliation (->> (peek child-content)
                            (filter map?)
                            first)]
    (>!! out-chan (with-meta account
                             {:record-type :account}))
    (when reconciliation
      (>!! out-chan (assoc reconciliation :account-id (:id account)))))
  nil)

(defmethod ^:private process-elem ::act/slots
  [{:keys [child-content]} _]
  (peek child-content))

(defmethod ^:private process-elem ::act/commodity
  [{:keys [child-content]} _]
  [[:commodity (into {} (peek child-content))]])

(defmethod ^:private process-elem ::gnc/budget
  [{:keys [out-chan child-content]} _]
  (>!! out-chan (with-meta (reduce (fn [r [k v]]
                                     (if (= :item k)
                                       (update-in r [:items] conj v)
                                       (if (blank-string? v)
                                         r
                                         (assoc r k v))))
                                   {:items []}
                                   (peek child-content))
                           {:record-type :budget}))
  nil)

(defmethod ^:private process-elem ::bgt/slots
  [{:keys [child-content]} _]
  (peek child-content))

(defmethod ^:private process-elem ::slot/value
  [{:keys [content child-content elems]} {:keys [tag]}]
  (let [tag-stack (map :tag elems)]
    (cond

      (= (take-last 4 tag-stack) ; budget periods
         [::gnc/budget
          ::bgt/slots
          :slot
          ::slot/value]) [[:periods (peek child-content)]]

      (= (take-last 4 tag-stack) ; reconciliation
         [::gnc/account
          ::act/slots
          :slot
          ::slot/value]) (peek child-content)

      :else
      [[(tag->keyword tag)
        (agg-text-content (peek content))]])))

; slot elements are used as generic key-value pair containers
; in the gnucash XML. It's necessary to inspect the parent
; chain in order to know how to handle the content
(defmethod ^:private process-elem :slot
  [{:keys [elems child-content]} _]
  (let [tag-stack (map :tag elems)]
    (cond
      (= (take-last 3 tag-stack) ; budget period
         [::gnc/budget
          ::bgt/slots
          :slot]) [[:item (into {} (peek child-content))]]

      (= (take-last 5 tag-stack) ; budget period item
         [::gnc/budget
          ::bgt/slots
          :slot
          ::slot/value
          :slot]) [(into {} (peek child-content))]

      (and (= (take-last 3 tag-stack) ; reconcile info
              [::gnc/account
               ::act/slots
               :slot])
           (= "reconcile-info"
              (-> child-content
                  peek
                  first
                  second))) [(with-meta (->> (peek child-content)
                                             (filter map?)
                                             (map (juxt (comp keyword :key) :value))
                                             (into {}))
                                        {:record-type :reconciliation})]

      (= (take-last 5 tag-stack) ; include-children or last-date
         [::gnc/account
          ::act/slots
          :slot
          ::slot/value
          :slot]) [(into {} (peek child-content))])))

(defmethod ^:private process-elem ::gnc/transaction
  [{:keys [out-chan child-content]} _]
  (>!! out-chan (with-meta (reduce (fn [r [k v]]
                                     (if (= :split k)
                                       (update-in r [:splits] conj v)
                                       (if (blank-string? v)
                                         r
                                         (assoc r k v))))
                                   {:splits []}
                                   (peek child-content))
                           {:record-type :transaction}))
  nil)

(defmethod ^:private process-elem ::trn/splits
  [{:keys [child-content]} _]
  (peek child-content))

(defmethod ^:private process-elem ::trn/split
  [{:keys [child-content]} _]
  [[:split (into {} (peek child-content))]])

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

; TODO: This can probably be simplified
(defn- push-child-content
  [lst content-coll]
  (reduce #(if (seq %2)
             (conj (pop %1)
                   (conj (peek %1) %2))
             %1)
          lst
          content-coll))

(defn- template?
  [elems]
  (let [tag-set (set (map :tag elems))]
    (tag-set ::gnc/template-transactions)))

(defmethod ^:private process-event EndElementEvent
  [{:keys [elems] :as state} _]
  (let [child-content (if (template? elems)
                        nil ; don't process templates
                        (process-elem state (peek elems)))]
    (-> state
        (update-in [:elems] pop)
        (update-in [:content] pop)
        (update-in [:child-content] pop)
        (update-in [:child-content] push-child-content child-content))))

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
        k [:prices exchange symbol]
        interval (t/months 1)
        last-trade-date (get-in @state [k])
        ignore? (when last-trade-date
                 (t/before? trade-date
                            (t/minus last-trade-date interval)))]
    (if ignore?
      (with-meta {} (merge (meta price) {:ignore? true}))
      (do
        (swap! state assoc-in [k] trade-date)
        (-> price
            (assoc :trade-date trade-date
                   :price (-> price :value parse-decimal)
                   :exchange exchange
                   :symbol symbol)
            (dissoc :time :value :commodity :currency))))))

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
  (-> (t/plus (t/epoch) (t/seconds seconds))
      (t/to-time-zone (t/time-zone-for-id "America/Chicago")) ; TODO: Need to know the time zone for the file for this
      tc/to-local-date))

(defmethod ^:private process-record :reconciliation
  [reconciliation _]
  (-> reconciliation
      (rename-keys {:last-date :end-of-period})
      (assoc :id (s/replace (str (uuid)) #"-" ""))
      (update-in-if [:include-children] parse-bool)
      (update-in [:end-of-period] (comp seconds-to-date
                                        parse-integer))))

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
                (map #(if (pred %)
                        (-> %
                            (update-in [:quantity] abs)
                            (update-in [:value] abs))
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
                                #(update-in % [0] parse-integer)
                                (juxt :key :value)))
                     (into {}))]
    (-> item
        (rename-keys {:key :account-id})
        (assoc :periods (map #(get-in periods [%] 0M)
                             (range period-count))))))

(defmethod ^:private process-record :budget
  [{:keys [num-periods] :as record} _]
  (let [period-count (parse-integer num-periods)]
    (-> record
        (assoc :period (-> record :recurrence :period_type keyword)
               :start-date (-> record :recurrence :start :gdate parse-date)
               :period-count period-count)
        (update-in [:items] (fn [items]
                              (map #(process-budget-item % period-count)
                                   items)))
        (select-keys [:period :period-count :start-date :items :id :name]))))

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
    :parse-fn parse-integer
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
