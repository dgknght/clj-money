(ns clj-money.import.gnucash
  (:refer-clojure :exclude [update])
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clojure.string :as s]
            [clojure.tools.logging :as log]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.core.async :refer [chan go go-loop <! <!! >!! >!]]
            [clj-time.core :as t]
            [clj-xpath.core :refer :all]
            [clj-money.core]
            [clj-money.util :refer [pprint-and-return
                                    pprint-and-return-l]]
            [clj-money.import :refer [read-source]])
  (:import [java.util.zip GZIPInputStream
                          GZIPOutputStream]
           [java.io File FileInputStream
                         FileOutputStream]))

(defn- parse-date
  [string-date]
  (when-let [match (re-find #"^(\d{4})-(\d{2})-(\d{2})" string-date)]
    (apply t/local-date (->> match
                             rest
                             (map #(Integer. %))))))

(defn- parse-decimal
  [string-decimal]
  (when-let [match (re-find #"(\d+)\/(\d+)" string-decimal)]
    (apply / (->> match
                  rest
                  (map bigdec)))))

(def ^:private namespace-map
  (->> ["gnc" "act" "trn" "ts" "split" "bgt" "recurrence" "slot" "cd" "cmdty" "price"]
       (map #(vector % (format "http://www.gnucash.org/XML/%s" %)))
       (into {})))

(def ^:private account-types-map
  {"ASSET"      :asset
   "BANK"       :asset
   "INCOME"     :income
   "EXPENSE"    :expense
   "LIABILITY"  :liability
   "CREDIT"     :liability
   "STOCK"      :asset})

(def ^:private ignored-accounts #{"Root Account" "Assets" "Liabilities" "Equity" "Income" "Expenses"})

(declare process-node)

(defn- process-node-attribute
  [node result {:keys [attribute xpath transform-fn default]}]
  (let [transform-fn (if transform-fn
                       transform-fn
                       identity)
        raw-value ($x:text? xpath node)
        value (if raw-value
                 (transform-fn raw-value)
                default)]
    (if (nil? value)
      result
      (assoc result attribute value))))

(defn- node->model
  [node attributes]
  (with-namespace-context namespace-map
    (reduce (partial process-node-attribute node) {} attributes)))

(defn- refine-account
  [account _]
  (-> account
      (assoc :commodity {:exchange (when-let [exchange (:commodity-exchange account)]
                                     (-> exchange
                                         s/lower-case
                                         keyword))
                         :symbol (:commodity-symbol account)})
      (dissoc :commodity-exchange :commodity-symbol)
      (vary-meta assoc :ignore? (contains? ignored-accounts (:name account)))))

(defn- refine-budget
  [budget node]
  (with-namespace-context namespace-map
    (assoc budget :items (map process-node ($x "bgt:slots/slot" node)))))

(defn- node->budget-item-period
  [node]
  (node->model node [{:attribute :index
                      :xpath "slot:key"
                      :transform-fn #(Integer. %)}
                     {:attribute :amount
                      :xpath "slot:value"
                      :transform-fn parse-decimal}]))

(defn- refine-budget-item
  [budget-item node]
  (with-namespace-context namespace-map
    (assoc budget-item :periods (->> node
                                     ($x "slot:value/slot")
                                     (map node->budget-item-period)
                                     (into #{})))))

(defn- refine-commodity
  [commodity _]
  (cond-> commodity
    (nil? (:name commodity))
    (assoc :name (:symbol commodity))

    (nil? (#{:nasdaq :nyse} (:exchange commodity)))
    (dissoc :exchange)

    (= :template (:exchange commodity))
    (vary-meta assoc :ignore? true)))

(defn- append-trading-attributes
  [transaction node]
  (with-namespace-context namespace-map
    (if (= :buy (:action transaction))
      (let [commodity-item-node (first ($x "trn:splits/trn:split[split:action = \"Buy\"]" node))
            commodity-account-node (first ($x (format "//gnc:book/gnc:account[act:id = \"%s\"]"
                                                      ($x:text "split:account" commodity-item-node))
                                              node))
            symbol ($x:text "act:commodity/cmdty:id" commodity-account-node)
            exchange (keyword (s/lower-case ($x:text "act:commodity/cmdty:space" commodity-account-node)))]
        (assoc transaction
               :shares (parse-decimal ($x:text "split:quantity" commodity-item-node))
               :commodity-account-id ($x:text "split:account" commodity-item-node)
               :account-id ($x:text "act:parent" commodity-account-node)
               :symbol symbol
               :exchange exchange))
      transaction)))

(defn- refine-transaction
  [transaction node]
  (with-namespace-context namespace-map
    (-> transaction
        (assoc :items (map process-node ($x "trn:splits/trn:split" node)))
        (append-trading-attributes node))))

(def ^:private translation-map
  {:gnc:account {:attributes [{:attribute :name
                           :xpath "act:name"}
                          {:attribute :type
                           :xpath "act:type"
                           :transform-fn #(get account-types-map % :equity)}
                          {:attribute :id
                           :xpath "act:id"}
                          {:attribute :parent-id
                           :xpath "act:parent"}
                          {:attribute :commodity-exchange
                           :xpath "act:commodity/cmdty:space"}
                          {:attribute :commodity-symbol
                           :xpath "act:commodity/cmdty:id"}]
                 :refine-fn refine-account
                 :meta {:record-type :account}}
   :gnc:budget {:attributes [{:attribute :id
                          :xpath "bgt:id"}
                         {:attribute :name
                          :xpath "bgt:name"}
                         {:attribute :start-date
                          :xpath "bgt:recurrence/recurrence:start/gdate"
                          :transform-fn parse-date}
                         {:attribute :period
                          :xpath "bgt:recurrence/recurrence:period_type"
                          :transform-fn keyword}
                         {:attribute :period-count
                          :xpath "bgt:num-periods"
                          :transform-fn #(Integer. %)}]
                :meta {:record-type :budget}
                :refine-fn refine-budget}
   :slot {:attributes [{:attribute :account-id ; Budget item
                        :xpath "slot:key"}]
          :refine-fn refine-budget-item}
   :budget-item-period {:attributes [{:attribute :index
                                      :xpath "slot:key"
                                      :transform-fn #(Integer. %)}
                                     {:attribute :amount
                                      :xpath "slot:value"
                                      :transform-fn parse-decimal}]}
   :gnc:commodity {:attributes [{:attribute :exchange
                             :xpath "cmdty:space"
                             :transform-fn (comp keyword s/lower-case)}
                            {:attribute :symbol
                             :xpath "cmdty:id"}
                            {:attribute :name
                             :xpath "cmdty:name"}
                            {:attribute :type
                             :xpath "cmdty:quote_source"
                             :transform-fn keyword
                             :default :stock}]
                   :meta {:record-type :commodity}
                   :refine-fn refine-commodity}
   :price {:attributes [{:attribute :trade-date
                         :xpath "price:time/ts:date"
                         :transform-fn parse-date}
                        {:attribute :price
                         :xpath "price:value"
                         :transform-fn parse-decimal}
                        {:attribute :exchange
                         :xpath "price:commodity/cmdty:space"
                         :transform-fn (comp keyword s/lower-case)}
                        {:attribute :symbol
                         :xpath "price:commodity/cmdty:id"}]
           :meta {:record-type :price}}
   :trn:split {:attributes [{:attribute :account-id
                             :xpath "split:account"}
                            {:attribute :reconciled
                             :xpath "split:reconciled-state"
                             :transform-fn #(= "y" %)}
                            {:attribute :amount
                             :xpath "split:value"
                             :transform-fn parse-decimal}
                            {:attribute :action
                             :xpath "split:value"
                             :transform-fn #(if (= \- (first %))
                                              :credit
                                              :debit)}]}
   :gnc:transaction {:attributes [{:attribute :transaction-date
                               :xpath "trn:date-posted/ts:date"
                               :transform-fn parse-date}
                              {:attribute :description
                               :xpath "trn:description"}
                              {:attribute :action
                               :xpath "trn:splits/trn:split/split:action"
                               :transform-fn (comp keyword s/lower-case)}]
                     :meta {:record-type :transaction}
                     :refine-fn refine-transaction}})

(defmulti ^:private process-node
  :tag)

(defmethod process-node :gnc:count-data
  [node]
  (let [record-type (-> node :attrs :cd:type keyword)]
  (-> {:record-type record-type
       :record-count (+ (Integer. (:text node))
                        (if (= :commodity record-type) 1 0))}
      (with-meta {:record-type :declaration}))))

(defmethod process-node :default
  [node]
  (let [{:keys [attributes
                refine-fn
                meta]} ((:tag node) translation-map)
        r-fn (if refine-fn
               #(refine-fn % node)
               identity)]
    (-> node
        (node->model attributes)
        (with-meta meta)
        r-fn)))

(def element-xpath
  (->> ["count-data" "account" "transaction" "budget" "commodity"]
       (map #(format "/gnc-v2/gnc:book/gnc:%s" %))
       (concat ["/gnc-v2/gnc:book/gnc:pricedb/price"])
       (s/join " | ")))

(defn- parse-input
  [input]
  (log/debug "reading the input stream into the DOM")
  ($x element-xpath (-> (GZIPInputStream. input)
                        io/reader
                        slurp
                        xml->doc)))

(defmethod read-source :gnucash
  [_ input]
  (with-namespace-context namespace-map
    (log/debug "processing the DOM source")
    (let [result (->> (parse-input input)
                      (map process-node)
                      (filter identity))]
      (log/debug "finished processing the DOM source")
      result)))

(defn- process-output
  [{:keys [verbose
           records
           output-folder
           file-name
           output-paths
           progress-chan]
    :as context}]
  (if (seq records)
    (let [output-path (format "%s/%s_%s.edn.gz"
                              output-folder
                              file-name
                              (count output-paths))]
      (binding [*print-meta* true]
        (with-open [writer (io/writer (GZIPOutputStream. (FileOutputStream. output-path)))]
          (.write writer (prn-str records))))
      (>!! progress-chan [:output-path output-path])
      (-> context
          (assoc :records [])
          (update-in [:output-paths] #(conj % output-path))))
    context))

(defn- advance-context
  [{:keys [records max-record-count] :as context}]
  (if (>= (count records) max-record-count)
    (process-output context)
    context))

(def ^:private chunk-file-options
  [["-v" "--verbose" "Print a lot of details about what's going on"]
   ["-m" "--maximum MAXIMUM" "The maximum number of records to include in each output file"
    :parse-fn #(Integer/parseInt %)
    :default 1000]])

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

(defmulti ^:private keep-record?
  (fn [record _]
    (-> record meta :record-type)))

(defmethod ^:private keep-record? :default
  [_ _]
  true)

(defmethod ^:private keep-record? :price
  [{:keys [trade-date] :as record} filter-state]
  (let [key-fn (juxt :exchange :symbol)
        trade-date-key (key-fn record)
        last-trade-date (get-in @filter-state
                                [:trade-dates trade-date-key]
                                (t/local-date 1900 1 1))
        cut-off (t/plus last-trade-date (t/months 1))]
    (when (< 0 (compare trade-date cut-off))
      (swap! filter-state #(assoc-in % [:trade-dates trade-date-key] trade-date))
      true)))

(defn- filter-record
  [xf]
  (let [filter-state (atom {})]
    (fn [result record]
      (if (keep-record? record filter-state)
        (xf result record)
        result))))

(defn- process-chunk-record
  "Processes a single record for the file chunking process"
  [{:keys [progress-chan] :as context} record]
  (>!! progress-chan [:record record])
  (-> context
      (update-in [:records] #(conj % record))
      advance-context))

(defn chunk-file
  "Accepts a path to a gnucash file and creates multiple, smaller files
  that container the same data, returning the paths to the new files"
  [& args]
  (let [{:keys [input-file
                output-folder
                file-name]
         :as opts} (parse-chunk-file-opts args)
        progress-chan (chan)
        result (go
                 (with-open [input-stream (FileInputStream. input-file)]
                   (->> (read-source :gnucash input-stream)
                        (transduce
                          filter-record
                          process-chunk-record
                          {:output-paths []
                           :verbose (-> opts :options :verbose)
                           :records []
                           :max-record-count (-> opts :options :maximum)
                           :file-name (second (re-matches #"^(.+)(\..+)$" file-name))
                           :output-folder output-folder
                           :progress-chan progress-chan})
                        process-output)))]
    (go-loop [value (<! progress-chan)]
             (case (first value)
               :record
               (when (not (:verbose opts))
                 (print (-> value second meta :record-type name first))
                 (flush))
               :output-path
               (println "\n" (second value)))
             (recur (<! progress-chan)))
    (println "reading the source file...")
    (<!! result)
    (println "\nDone")))
