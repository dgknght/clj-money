(ns clj-money.ingestion.receipts
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
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
                  :payment_account (account-property
                                     "The account that best matches the payment method"
                                     payment-accounts)
                  :line_items {:type "array"
                               :items {:type "object"
                                       :properties {:description {:type "string"}
                                                    :amount {:type "number"}
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
     "- *line_items* If the receipt includes this level of detail. For each, choose the expense account which best matches the item description."
     "- *payment_account* Select the enum value that best matches the payment method."
     "- *tax* Total total tax listed on the receipt."
     ""
     "When selecting an expense account, following these guidelines:"
     "- If the merchant is a restaurant, prefer \"Dining\" over the \"Groceries\" accounts"
     "- If the merchant is a market or big box store, prefer \"Groceries\" accounts over \"Dining\""]))
