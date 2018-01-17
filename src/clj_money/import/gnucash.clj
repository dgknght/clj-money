(ns clj-money.import.gnucash
  (:refer-clojure :exclude [update])
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clojure.string :as s]
            [clojure.tools.logging :as log]
            [clojure.tools.cli :refer [parse-opts]]
            [clj-time.core :as t]
            [clj-xpath.core :refer :all]
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

(defn- refine-account
  [account]
  (-> account
      (assoc :commodity {:exchange (when-let [exchange (:commodity-exchange account)]
                                     (-> exchange
                                         s/lower-case
                                         keyword))
                         :symbol (:commodity-symbol account)})
      (dissoc :commodity-exchange :commodity-symbol)
      (vary-meta assoc :ignore? (contains? ignored-accounts (:name account)))))

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
                :meta {:record-type :budget}}
   :budget-item {:attributes [{:attribute :account-id
                               :xpath "slot:key"}]}
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
                   :meta {:record-type :commodity}}
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
   :transaction-item {:attributes [{:attribute :account-id
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
                     :meta {:record-type :transaction}}})

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

(defmulti ^:private process-node
  :tag)

(defmethod process-node :gnc:count-data
  [node]
  (-> {:record-type (-> node :attrs :cd:type keyword)
       :record-count (Integer. (:text node))}
      (with-meta {:record-type :declaration})))

(defmethod process-node :default
  [node]
  (let [{:keys [attributes
                refine-fn
                meta]} ((:tag node) translation-map)]
    (-> node
        (node->model attributes)
        (with-meta meta)
        ((or refine-fn identity)))))

#_(defn- node->budget-item
  [node]
  (with-namespace-context namespace-map
    (-> node
        (node->model budget-item-attributes)
        (assoc :periods (->> node
                             ($x "slot:value/slot")
                             (map node->budget-item-period)
                             (into #{}))))))

#_(defmethod process-node :gnc:budget
  [node]
  (with-namespace-context namespace-map
    (-> node
        (node->model budget-attributes)
        (assoc :items (map node->budget-item ($x "bgt:slots/slot" node)))
        (with-meta {:record-type :budget}))))

#_(defn- refine-commodity
  [commodity]
  (cond-> commodity
    (nil? (:name commodity))
    (assoc :name (:symbol commodity))

    (nil? (#{:nasdaq :nyse} (:exchange commodity)))
    (dissoc :exchange)))

#_(defn- node->commodity
  [node]
  (let [commodity (node->model node commodity-attributes)]
    (when (not= :template (:exchange commodity))
      (-> commodity
          refine-commodity
          (with-meta {:record-type :commodity})))))

#_(defmethod process-node :price
  [node]
  (-> node
      (node->model price-attributes)
      (with-meta {:record-type :price})))

#_(defn- node->transaction
  [node]
  (with-namespace-context namespace-map
    (-> node
        (node->model transaction-attributes)
        (assoc :items (->> ($x "trn:splits/trn:split" node)
                           (map node->transaction-item))))))

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

#_(defmethod process-node :gnc:transaction
  [node]
  (-> node
      node->transaction
      (append-trading-attributes node)
      (with-meta {:record-type :transaction})))

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
  [{:keys [verbose records output-folder file-name output-paths] :as context}]
  (if (seq records)
    (let [output-path (format "%s/%s_%s.edn.gz"
                              output-folder
                              file-name
                              (count output-paths))]

      (log/info "Writing" (count records) "records to" output-path)

      (with-open [writer (io/writer (GZIPOutputStream. (FileOutputStream. output-path)))]
        (.write writer (prn-str records)))

      (log/info "Finished writing" output-path)

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

(defn chunk-file
  "Accepts a path to a gnucash file and creates multiple, smaller files
  that container the same data, returning the paths to the new files"
  [& args]
  (let [opts (parse-opts args chunk-file-options)
        input-path (-> opts :arguments first)
        input-file (File. input-path)
        output-folder (.getParent input-file)
        file-name (.getName input-file)
        context (atom {:output-paths []
                       :verbose (:vervose opts)
                       :records []
                       :max-record-count (-> opts :options :maximum)
                       :file-name (second (re-matches #"^(.+)(\..+)$" file-name))
                       :output-folder output-folder})]
    (with-open [input-stream (FileInputStream. input-file)]
      (doseq [record (read-source :gnucash input-stream)]
        (when (not (:verbose opts))
          (print "."))
        (swap! context #(-> %
                            (update-in [:records] (fn [records] (conj records record)))
                            advance-context)))
      (process-output @context)))
  (println ""))
