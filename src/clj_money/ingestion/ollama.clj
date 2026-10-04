(ns clj-money.ingestion.ollama
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [cheshire.core :as json]
            [lambdaisland.uri :as uri]
            [clj-http.client :as http]
            [clj-money.ingestion :as ing]
            [clj-money.entities :as ents]
            [clj-money.accounts :refer [nest unnest]])
  (:import java.util.Base64))

(defn- ->base64
  [input]
  (let [input (io/input-stream input)]
    (.encodeToString (Base64/getEncoder)
                     (.readAllBytes input))))

(defn- url
  [{:keys [host
           port
           scheme]
    :or {host "localhost"
         port 11434
         scheme "http"}}]
  (-> (uri/parse "/api/generate")
      (assoc :host host
             :port port
             :scheme scheme)
      uri/uri-str))

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

(defn- schema
  "Returns a JSON schema describing the expected response. Ollama constrains
  the model output to match it, so the account fields can only contain one of
  the given account paths."
  [{:keys [payment-accounts expense-accounts]}]
  {:type "object"
   :properties {:date {:type "string"
                       :description "The purchase date in YYYY-MM-DD format"}
                :location {:type "string"
                           :description "The name of the merchant"}
                :total {:type "number"}
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
   :required ["date" "location" "total" "payment_account" "line_items"]})

(def ^:private prompt
  "This is a purchase receipt. Extract the date, merchant, total amount, and line items. If there are no line items, return a single line item for the total. For payment_account, choose the account that best matches the payment method shown on the receipt. For each line item, choose the expense account that best matches the item.")

(defn- request-body
  [image entity {:keys [model num-ctx]
                 :or {model "qwen2.5vl:7b"
                      num-ctx 8192}}]
  {:model model
   :prompt prompt
   :stream false
   :format (let [x (schema (account-options entity))]

             (pprint {::schema x})

             x)
   :options {:temperature 0
             :num_ctx num-ctx}
   :images [image]})

(defn- read-receipt*
  [source entity opts]
  (let [req-body (-> source
                       ->base64
                       (request-body entity opts))
        req {:content-type "application/json"
             :accept "application/json"
             :as :json
             :body (json/generate-string req-body)}
        {:keys [status body]} (http/post (url opts)
                                         req)]
    (if (<= 200 status 299)
      (do
        (log/debugf "format: %s" (pr-str (:format req-body)))
        (log/debugf "result: %s" (with-out-str (pprint (dissoc body :context))))
        (when (<= (get-in req-body [:options :num_ctx])
                  (:prompt_eval_count body 0))
          (log/warnf "The prompt filled the context window (%s tokens) and may have been truncated"
                     (:prompt_eval_count body)))
        (update-in (dissoc body :context)
                   [:response]
                   #(json/parse-string % true)))
      (do
        (log/errorf "Error accessing the ollama service: %s" body)
        (throw (ex-info "Error accessing the ollama service." {:source source}))))))

(defmethod ing/reader ::ollama
  [config]
  (reify ing/Reader
    (read-receipt
      [_ source entity]
      (read-receipt* source
                     entity
                     config))))
