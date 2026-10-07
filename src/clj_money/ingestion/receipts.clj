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
  [entity]
  {:payment-accounts (mapv :account/name
                           (ents/select {:account/user-tags :payment-method
                                         :account/entity entity}))
   :expense-accounts (->> (ents/select {:account/type :expense
                                        :account/entity entity})
                          nest
                          unnest
                          (filter leaf-account?)
                          paths)})

(defn- account-property
  [description paths]
  (cond-> {:type "string"
           :description description}
    (seq paths) (assoc :enum paths)))

(defn schema
  "Returns a JSON schema describing the expected response. Ollama constrains
  the model output to match it, so the account fields can only contain one of
  the given account paths."
  [entity]
  (let [{:keys [payment-accounts expense-accounts]} (account-options entity)]
    {:type "object"
     :properties {:date {:type "string"
                         :description "The purchase date in YYYY-MM-DD format"}
                  :location_name {:type "string"
                                  :description "The name of the merchant"}
                  :location_address {:type "string"
                                     :description "The address of the merchant"}
                  :total {:type "number"}
                  :tax {:type "number"}
                  :tax-rate {:type "number"}
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
                "payment_account"]}))

(defn prompt
  "Generates a prompt to read a receipt and return structured data"
  [_entity]
  (str/join
    "\n"
    ["This is a purchase receipt. Extract the the following:"
     "- *location_name* The name of the merchant. If unable to find it, \"unknown\"."
     "- *location_address* The physical address of the merchant. If unable to find it, \"unknown\"."
     "- *transaction date* The date on which the transaction took place."
     "- *total* The total amount paid."
     "- *line_items* If the receipt includes this level of detail. For each,"
     "  choose the expense account which best matches the item description."
     "  Some receipts (e.g., for grocery stores) indicate if a line item is"
     "  taxable, often with a \"T\"."
     "- *payment_account* Select the enum value that best matches the payment method."
     "- *tax* Total tax listed on the receipt. (May not be present.)"
     "- *tax-rate* Tax rate listed on the receipt. (May not be present.)"
     ""
     "When selecting an expense account, following these guidelines:"
     "- If the merchant is a restaurant, prefer \"Dining\" over the \"Groceries\" accounts"
     "- If the merchant is a market or big box store, prefer \"Groceries\" accounts over \"Dining\""]))

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
  #:transaction{:transaction-date (t/local-date (t/formatter "MM-dd-yyyy") date)
                :description location-name
                :items (cons (payment-item receipt entity)
                             (translate-items entity receipt))})
