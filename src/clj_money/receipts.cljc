(ns clj-money.receipts
  (:require #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            [clojure.spec.alpha :as s]
            [clj-money.decimal :as d]
            [clj-money.dates :as dates]))

(defn total
  [{:receipt/keys [items]}]
  (->> items
       (remove empty?)
       (map :receipt-item/quantity)
       (reduce d/+ 0M)))

(defn- item-polarity-aligns?
  [{:receipt/keys [items]}]
  (let [grouped (->> items
                     (map :receipt-item/quantity)
                     (filter identity)
                     (group-by pos?))]
    (or (empty? (grouped true))
        (empty? (grouped false)))))

(s/def ::id (s/or :uuid uuid? :string string? :int int? :key keyword?)) ; keyword is for testing
(s/def ::model-ref (s/keys :req-un [::id]))
(s/def :receipt-item/quantity d/decimal?)
(s/def :receipt-item/account ::model-ref)
(s/def :receipt-item/memo (s/nilable string?))
(s/def ::receipt-item (s/keys :opt [:receipt-item/account
                                    :receipt-item/quantity
                                    :receipt-item/memo]))
(s/def :receipt/transaction-date dates/local-date?)
(s/def :receipt/description string?)
(s/def :receipt/payment-account ::model-ref)
(s/def :receipt/items (s/coll-of ::receipt-item))
(s/def :receipt/transaction-id ::id)
(s/def ::receipt (s/and (s/keys :req [:receipt/transaction-date
                                      :receipt/description
                                      :receipt/payment-account
                                      :receipt/items]
                                :opt [:receipt/transaction-id])
                        item-polarity-aligns?))

(defn- ->transaction-item
  [action]
  (fn [{:receipt-item/keys [account quantity memo]}]
    #:transaction-item{:account account
                       :quantity (d/abs quantity)
                       :memo memo
                       :action action}))

(defn- empty-item?
  [{:receipt-item/keys [quantity account]}]
  (or (nil? quantity)
      (nil? account)))

(defn ->transaction
  [{:receipt/keys [transaction-date
                   transaction-id
                   description
                   payment-account
                   items]
    :as receipt}]
  {:pre [(s/valid? ::receipt receipt)]}
  (let [total (total receipt)
        [payment-action item-action] (if (< 0M total)
                                       [:credit :debit]
                                       [:debit :credit])]
    (cond-> #:transaction{:description description
                          :transaction-date transaction-date
                          :items (cons #:transaction-item{:account payment-account
                                                          :action payment-action
                                                          :quantity (d/abs total)}
                                       (->> items
                                            (remove empty-item?)
                                            (map (->transaction-item item-action))))}
      transaction-id (assoc :id transaction-id))))

(defn- <-transaction-item
  [{:transaction-item/keys [account quantity memo]}]
  #:receipt-item{:account account
                 :quantity quantity
                 :memo memo})

(defn <-transaction
  [{:transaction/keys [items transaction-date description] :as trx}]
  (let [{[payment :as payments] true expenses false} (group-by #(= :credit
                                                   (:transaction-item/action %))
                                               items)]
    (assert (= 1 (count payments))
            "Expected one payment item, but found more")
    #:receipt{:transaction-date transaction-date
              :transaction-id (:id trx)
              :description description
              :payment-account (:transaction-item/account payment)
              :items (mapv <-transaction-item expenses)}))
