(ns clj-money.receipts
  (:require #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            [clojure.spec.alpha :as s]
            [clj-money.decimal :as d]
            [clj-money.dates :as dates]))

(defn- item-polarity-aligns?
  [{:receipt/keys [items]}]
  (let [grouped (->> items
                     (map :receipt-item/quantity)
                     (filter identity)
                     (group-by pos?))]
    (or (empty? (grouped true))
        (empty? (grouped false)))))

(s/def ::id (s/or :string string? :int int? :key keyword?)) ; keyword is for testing
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
  (let [total (->> items
                   (map :receipt-item/quantity)
                   (filter identity)
                   (reduce d/+ 0M))
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
