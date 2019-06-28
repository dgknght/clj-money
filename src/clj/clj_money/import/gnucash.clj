(ns clj-money.import.gnucash
  (:refer-clojure :exclude [update])
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clojure.string :as s]
            [clojure.tools.logging :as log]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.core.async
             :refer [pipe
                     sliding-buffer
                     chan
                     go
                     go-loop
                     close!
                     <!
                     <!!
                     >!!
                     >!]
             :as async]
            [clojure.data.xml :as xml]
            [clj-time.core :as t]
            [clj-xpath.core :refer :all]
            [clj-money.core]
            [clj-money.util :refer [pprint-and-return
                                    pprint-and-return-l]]
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
  (when-let [match (re-find #"(\d+)\/(\d+)" string-decimal)]
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
  [state elem]
  #_(pprint {:process-elem-default (:tag elem)})
  nil)

(defn- tag->keyword
  [tag]
  (keyword (last (s/split (name tag) #"\/"))))

; For simple elements that represent attributes of their
; containing elemen, return a name/value pair that will
; later be turned into a map representing the record
(defmethod ^:private process-elem :attribute
  [{:keys [content] :as state} {tag :tag}]
  [[(tag->keyword tag)
    (agg-text-content (peek content))]])

(defmethod ^:private process-elem :compound-attribute
  [{:keys [child-content]} {:keys [tag]}]
  [[(tag->keyword tag)
    (into {} (peek child-content))]])

(defmethod ^:private process-elem ::gnc/count-data
  [{:keys [out-chan content] :as state} {:keys [attrs]}]
  (let [record-type (keyword (::cd/type attrs))
        record-count (parse-integer (agg-text-content (peek content)))
        record (-> {:record-type record-type
                    :record-count (+ record-count
                                     (if (= :commodity record-type) 1 0))}
                   (with-meta {:record-type :declaration}))]
    (>!! out-chan record)
    nil))

(defmethod ^:private process-elem ::gnc/commodity
  [{:keys [out-chan child-content] :as state} _]
  (>!! out-chan (with-meta (into {} (peek child-content))
                           {:record-type :commodity}))
  nil)

(defmethod ^:private process-elem :price
  [{:keys [out-chan child-content] :as state} _]
  (>!! out-chan (with-meta (into {} (peek child-content))
                           {:record-type :price}))
  nil)

(defmethod ^:private process-elem ::gnc/account
  [{:keys [out-chan child-content] :as state} _]
  (>!! out-chan (with-meta (into {} (peek child-content))
                           {:record-type :account}))
  nil)

(defmethod ^:private process-elem ::act/commodity
  [{:keys [child-content]} elem]
  [[:commodity (into {} (peek child-content))]])

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
  nil)

(defmethod ^:private process-elem ::bgt/slots
  [{:keys [child-content]} _]
  (peek child-content))

(defmethod ^:private process-elem ::slot/value
  [{:keys [content child-content]} {:keys [tag attrs]}]
  (if (= "frame" (:type attrs))
    [[:periods (peek child-content)]]
    [[(tag->keyword tag)
      (agg-text-content (peek content))]]))

; slot elements are used as generic key-value pair containers
; in the gnucash XML. It's necessary to inspect the parent
; chain in order to know how to handle the content
(defmethod ^:private process-elem :slot
  [{:keys [elems child-content content]} elem]
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
          :slot]) [(into {} (peek child-content))])))

(defmethod ^:private process-elem ::gnc/transaction
  [{:keys [out-chan child-content elems]} elem]
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
  [{:keys [child-content]} elem]
  (peek child-content))

(defmethod ^:private process-elem ::trn/split
  [{:keys [child-content]} elem]
  [[:split (into {} (peek child-content))]])

(defmulti ^:private process-event
  (fn [_ event]
    (type event)))

(defmethod ^:private process-event :default
  [_ event]
  #_(pprint {:event (type event)}))

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
  (reduce #(if (and %2 (seq %2))
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
  [{:keys [elems content out-chan] :as state} event]
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
  [commodity _]
  (let [c (-> commodity
              (rename-keys {:id :symbol
                            :space :exchange
                            :quote_source :type})
              (update-in [:type] keyword)
              (update-in [:exchange] (comp keyword s/lower-case)))]
    (cond-> c
      (nil? (:type c))
      (assoc :type :stock)

      (nil? (:name c))
      (assoc :name (:symbol c))

      (nil? (#{:nasdaq :nyse} (:exchange c)))
      (dissoc :exchange)

      (= "template" (:symbol c))
      (vary-meta assoc :ignore? true))))

(defmethod ^:private process-record :price
  [price state]
  (let [trade-date (-> price :time :date parse-date)
        exchange (-> price :commodity :space s/lower-case keyword)
        symbol (-> price :commodity :id)
        k [:prices exchange symbol]
        interval (t/weeks 1)
        last-trade-date (get-in @state [k])
        ignore? (when last-trade-date
                 (t/before? trade-date
                            (t/minus last-trade-date interval)))]
    (when-not ignore?
      (swap! state assoc-in [k] trade-date))
    (-> price
        (assoc :trade-date trade-date
               :price (-> price :value parse-decimal)
               :exchange exchange
               :symbol symbol)
        (dissoc :time :value :commodity :currency)
        (vary-meta assoc :ignore? ignore?))))

(defmethod ^:private process-record :account
  [{:keys [commodity] :as account} _]
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
                                                                (-> e
                                                                    s/lower-case
                                                                    keyword))))))
        (vary-meta assoc :ignore? (contains? ignored-accounts (:name account))))))

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

(defmethod ^:private refine-trading-transaction :default
  [transaction]
  (println "*** unknown trading transaction type ***")
  (println (->> (:items transaction)
                (map :action)
                set))
  (println "*** unknown trading transaction type ***")
  transaction)

(defmethod ^:private refine-trading-transaction :none [t] t)

(defn- adjust-trade-actions
  [items]
  (map (fn [item]
         (update-in item [:action] #(case %
                                      :buy :debit
                                      :sell :credit
                                      :split :debit
                                      %)))
       items))

(defmethod ^:private refine-trading-transaction :purchase
  [transaction]
  (let [commodity-item (->> (:items transaction)
                            (filter #(= :buy (:action %)))
                            first)]
    ; TODO: import is expecting :account-id, :symbol, and :exchange, all
    ;       of which can be looked up from the :commodity-account-id

    (-> transaction
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
    ; TODO: import is expecting :account-id, :symbol, and :exchange, all
    ;       of which can be looked up from the :commodity-account-id

    (-> transaction
        (assoc
          :trade-date (:transaction-date transaction)
          :action :sell
          :shares (:quantity commodity-item)
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
        (update-in [:items] adjust-trade-actions))))

(defn- process-transaction-item
  [item]
  (-> item
      (rename-keys {:account :account-id
                    :reconciled-state :reconciled})
      (update-in [:action] #(if %
                              (-> % s/lower-case keyword)
                              (if (= \- (first (:quantity item)))
                                :credit
                                :debit)))
      (update-in [:quantity] parse-decimal)
      (update-in [:value] parse-decimal)
      (update-in [:reconciled] #(= "y" %))))

(defmethod ^:private process-record :transaction
  [transaction _]
  (-> transaction
      (assoc :transaction-date (-> transaction :date-posted :date parse-date))
      (dissoc :date-posted)
      (rename-keys {:splits :items})
      (update-in [:items] #(map process-transaction-item %))
      refine-trading-transaction))

(defn- process-budget-item-period
  [period]
  (-> period
      (rename-keys {:key :index
                    :value :amount})
      (update-in [:index] parse-integer)
      (update-in [:amount] parse-decimal)))

(defn- process-budget-item
  [item]
  (-> item
      (rename-keys {:key :account-id})
      (update-in [:periods] #(->> %
                                  (map process-budget-item-period)
                                  set))))

(defmethod ^:private process-record :budget
  [record _]
  (-> record
      (rename-keys {:num-periods :period-count})
      (update-in [:period-count] parse-integer)
      (assoc :period (-> record :recurrence :period_type keyword)
             :start-date (-> record :recurrence :start :gdate parse-date))
      (update-in [:items] #(map process-budget-item %))
      (dissoc :recurrence)))

(defn- process-records
  [xf]
  (let [state (atom {})]
    (fn
      ([] (xf))
      ([acc] (xf acc))
      ([acc record]
       (xf acc (process-record record state))))))

(defmethod read-source :gnucash
  [_ inputs out-chan]
  (let [records-chan (chan 1000 (comp filter-records
                                      process-records))
        out-pipe (pipe records-chan out-chan)]
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

(defn- append-record
  [xf]
  (fn
    ([] (xf))
    ([context] (xf context))
    ([context record]
     (xf
       (update-in context [:records] #(conj % record))
       record))))

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
