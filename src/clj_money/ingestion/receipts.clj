(ns clj-money.ingestion.receipts
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [index-by]]
            [clj-money.decimal :as d]
            [clj-money.entities :as ents]
            [clj-money.accounts :refer [nest unnest]]))

(def ^:private leaf-account?
  (comp (some-fn nil? zero?)
        :child-count))

(defn- paths
  [accounts]
  (mapv (comp (partial str/join "/")
              :account/path)
        accounts))

(defn- account-options
  "Given a list of accounts, returns the account paths the model may choose
  from for the payment method and for the expenses."
  [{{:settings/keys [expense-accounts payment-methods]} :entity/settings :as entity}]
  (let [expense? (if (seq expense-accounts)
                   (comp (->> expense-accounts
                              (map :id)
                              (into #{}))
                         :id)
                   leaf-account?)]
    {:payment-accounts (->> payment-methods
                            ents/find-many
                            (mapv :account/name))
     :expense-accounts (->> (ents/select {:account/type :expense
                                          :account/entity entity})
                            nest
                            unnest
                            (filter expense?)
                            paths)}))

(defn- account-property
  [description paths]
  (cond-> {:type "string"
           :description description}
    (seq paths) (assoc :enum paths)))

(defn build-schema
  "Returns a JSON schema describing the expected response, given the account
  names the model may choose from. Ollama constrains the model output to match
  it, so the account fields can only contain one of the given account paths."
  [{:keys [payment-accounts expense-accounts]}]
  {:type "object"
   :properties {:date {:type ["string" "null"]
                       :description "The date of the purchase"}
                :location_name {:type ["string" "null"]
                                :description "The name of the merchant"}
                :location_address {:type ["string" "null"]
                                   :description "The address of the merchant"}
                :total {:type "number"}
                :tax {:type ["number" "null"]}
                :tax_rate {:type ["number" "null"]}
                :tip {:type ["number" "null"]}
                :payment_account (account-property
                                   "The account that best matches the payment method"
                                   payment-accounts)
                :line_items {:type "array"
                             :items {:type "object"
                                     :properties {:description {:type "string"}
                                                  :amount {:type "number"}
                                                  :taxable {:type "boolean"}
                                                  :account (account-property
                                                             "The expense account that best matches the item"
                                                             expense-accounts)}
                                     :required ["description" "amount" "account"]}}}
   :required ["date"
              "location_name"
              "location_address"
              "total"
              "tax"
              "payment_account"]})

(defn schema
  "Returns a JSON schema describing the expected response, offering the
  entity's accounts as the choices for the account fields."
  [entity]
  (build-schema (account-options entity)))

(defn prompt
  "Generates a prompt to read a receipt and return structured data"
  [{{:settings/keys [expense-hints]} :entity/settings}]
  (let [base ["This is a purchase receipt. Extract the following."
              "- *location_name* The name of the merchant."
              "- *location_address* The physical address of the merchant."
              "- *date* The date on which the transaction took place, written as YYYY-MM-DD."
              "- *total* The final amount charged, including any tip. When the receipt"
              "  shows a tip, this is the amount after the tip was added (e.g., the"
                                                                                "  \"Authorized Amount\" or a handwritten total), not the order total before"
              "  the tip."
              "- *tip* The tip or gratuity, or null if there is none."
              "- *tax* The total sales tax charged, from the line labeled TAX or Sales Tax"
              "  (0 if that line shows 0.00), or null if the receipt has no tax line."
              "- *tax_rate* The tax rate as a decimal fraction (e.g., 8.25% is 0.0825),"
              "  only if a rate is printed on the receipt; otherwise null. Do not"
              "  calculate it."
              "- *payment_account* The enum value that matches the card brand or payment"
              "  method printed on the receipt (e.g., \"Discover\" when the receipt says"
                                                      "  DISCOVER). Choose a cash account only if the receipt shows a cash payment."
              "- *line_items* One entry for each product or service purchased, with the"
              "  price actually paid for it (after any discount or savings shown for that"
                                                   "  item, e.g., a \"You Pay\" column). Leave out lines with no price or a price"
              "  of 0.00 (e.g., toppings), and lines that are not purchases: subtotal, tax,"
              "  tip, total, amount, discounts, savings, and payment lines. If the receipt"
              "  does not list what was purchased, return an empty list. Some receipts"
              "  (e.g., for grocery stores) mark taxable items, often with a \"T\"."]]
    (if (seq expense-hints)
      (str/join "\n"
                (apply conj
                       base
                       ""
                       "When selecting an expense account, follow these guidelines:"
                       (map #(str "- %s" %) expense-hints)))
      (str/join "\n" base))))

(defn- accounts-by-path
  [entity]
  (->> (ents/select
         {:account/type [:in [:expense]]
          :account/entity entity})
       nest
       unnest
       (index-by (comp #(str/join "/" %)
                       :account/path))))

(defn- translate-items
  [entity {:keys [line-items tax-rate]}]
  (let [accounts (accounts-by-path entity)]
    (->> line-items
         (map (comp #(assoc % :total (+ (:amount %)
                                        (:tax-amount %)))
                    #(assoc % :tax-amount (* (:tax-rate %)
                                             (:amount %)))
                    #(assoc % :tax-rate (if (:taxable %)
                                          tax-rate
                                          0.0M))))
         (group-by :account)
         (mapv (comp
                 (fn [[account items]]
                   #:transaction-item{:account account
                                      :quantity (d/round
                                                  (->> items
                                                       (map :total)
                                                       (reduce + 0M))
                                                  2)
                                      :action :debit})
                 #(update-in % [0] accounts))))))

(defn- payment-item
  [{:keys [total
           payment-account]}
   entity]
  #:transaction-item{:account (ents/find-by {:account/name payment-account
                                             :account/entity entity}) 
                     :quantity (bigdec total)
                     :action :credit})

(defn make-trx
  [{:keys [location-name
           date] :as receipt}
   entity]
  #:transaction{:transaction-date (t/local-date (t/formatter "yyyy-MM-dd") date)
                :description location-name
                :items (cons (payment-item receipt entity)
                             (translate-items entity receipt))})
