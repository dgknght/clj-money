(ns clj-money.test-context
  (:refer-clojure :exclude [find])
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clojure.java.io :as io]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.io :refer [read-bytes]]
            [clj-money.util :as util :refer [model=]]
            [clj-money.models :as models]
            [clj-money.transactions :refer [expand]]))

(def ^:dynamic *context* nil)

(def basic-context
  [#:user{:email "john@doe.com"
          :first-name "John"
          :last-name "Doe"
          :password "Please001!"}
   #:user {:email "jane@doe.com"
           :first-name "Jane"
           :last-name "Doe"
           :password "Please001!"}
   #:entity{:name "Personal"
            :user "john@doe.com"}
   #:entity{:name "Business"
            :user "jane@doe.com"}
   #:commodity{:name "US Dollar"
               :entity "Personal"
               :symbol "USD"
               :type :currency}
   #:commodity{:name "US Dollar"
               :entity "Business"
               :symbol "USD"
               :type :currency}
   #:account{:name "Opening Balances"
             :type :equity
             :entity "Personal"}
   #:account{:name "Checking"
             :entity "Personal"
             :type :asset}
   #:account{:name "Salary"
             :entity "Personal"
             :type :income}
   #:account{:name "Rent"
             :entity "Personal"
             :type :expense}
   #:account{:name "Groceries"
             :entity "Personal"
             :type :expense}
   #:account{:name "Tax"
             :entity "Personal"
             :type :expense}
   #:account{:name "FIT"
             :type :expense
             :entity "Personal"
             :parent "Tax"}
   #:account{:name "Medicare"
             :type :expense
             :entity "Personal"
             :parent "Tax"}
   #:account{:name "Sales"
             :entity "Business"
             :type :income}])

(defn- kv-pred
  [& kvs]
  {:pre [(even? (count kvs))]}
  (apply every-pred
         (->> kvs
              (partition 2)
              (mapv (fn [[k v]]
                      #(= v (k %)))))))

(defn- find
  ([context k v & kvs]
   {:pre [context k v]}
   (or (find context (apply kv-pred k v kvs))
       (do
         (pprint {::context context})
         (throw (ex-info "Model not found" (->> kvs
                                                (concat [k v])
                                                (partition-all 2)
                                                (map vec)
                                                (into {})))))))
  ([context f]
   (->> context
        (filter f)
        first)))

(defn find-user
  ([email] (find-user *context* email))
  ([context email]
   (find context :user/email email)))

(defn- context+
  [args]
  (if (map? (first args))
    args
    (cons *context* args)))

(defn find-entity
  ([entity-name] (find-entity *context* entity-name))
  ([context entity-name]
   {:pre [context entity-name]}
   (find context :entity/name entity-name)))

(defn find-import
  ([entity-name] (find-import *context* entity-name))
  ([context entity-name]
   (find context :entity-name entity-name)))

(defn find-imports
  [& args]
  (let [[context & entity-names] (context+ args)]
    (map #(find-import context %) entity-names)))

(defn find-grant
  ([entity user] (find-grant *context* entity user))
  ([context entity user]
   (find context :grant/entity entity :grant/user user)))

(defn find-account
  ([account-name] (find-account *context* account-name))
  ([context account-name]
   (find context :account/name account-name)))

(defn find-accounts
  [& args]
  (let [[context & account-names] (context+ args)]
    (map #(find-account context %) account-names)))

(defn find-attachment
  ([caption] (find-attachment *context* caption))
  ([context caption]
   (find context :attachment/caption caption)))

(defn find-image
  ([original-filename] (find-image *context* original-filename))
  ([context original-filename]
   (find context :image/original-filename original-filename)))

(defn find-commodity
  ([symbol] (find-commodity *context* symbol))
  ([context symbol]
   (find context :commodity/symbol symbol)))

(defn find-commodities
  [& args]
  (let [[context & symbols] (context+ args)]
    (map #(find-commodity context %) symbols)))

(defn find-budget
  ([budget-name] (find-budget *context* budget-name))
  ([context budget-name]
   (find context :budget/name budget-name)))

(defn find-price
  ([sym trade-date] (find-price *context* sym trade-date))
  ([context sym trade-date]
   (let [commodity (find-commodity context sym)]
     (find context #(and (= (:id commodity) (get-in % [:price/commodity :id]))
                         (= trade-date (:price/trade-date %)))))))

(defn find-transaction
  ([identifier] (find-transaction *context* identifier))
  ([context [transaction-date description]]
   {:pre [(string? description) (t/local-date? transaction-date)]}

   (find context
         :transaction/transaction-date transaction-date
         :transaction/description description)))

(defn find-transaction-item
  ([identifier]
   (find-transaction-item *context* identifier))
  ([context [transaction-date quantity account]]
   (let [act (if (map? account)
               account
               (find-account context account))]
     (->> context
          (filter #(= transaction-date (:transaction/transaction-date %)))
          (mapcat :transaction/items)
          (filter #(and (model= act (:transaction-item/account %))
                        (= quantity (:transaction-item/quantity %))))
          first))))

(defn find-scheduled-transaction
  ([description] (find-scheduled-transaction *context* description))
  ([context description]
   (find context :scheduled-transaction/description description)))

(defn find-reconciliation
  ([identifier]
   (find-reconciliation *context* identifier))
  ([ctx [account end-of-period]]
   (let [act (util/->model-ref (if (map? account)
                                 account
                                 (find-account ctx account)))]
     (find ctx
           :reconciliation/account act
           :reconciliation/end-of-period end-of-period))))

#_(defn- execute-trade
  [trade context]
  {:pre [(#{:buy :purchase :sell :sale} (:type trade))]}

  (let [f (case (:type trade)
            :buy      trading/buy
            :purchase trading/buy
            :sell     trading/sell
            :sale     trading/sell)]
    (-> trade
        (resolve-entity context :entity)
        (resolve-account context)
        (resolve-account context :lt-capital-gains-account-id)
        (resolve-account context :st-capital-gains-account-id)
        (resolve-account context :lt-capital-loss-account-id)
        (resolve-account context :st-capital-loss-account-id)
        (resolve-commodity context :trade/commodity)
        f)))

(defmulti ^:private prepare
  (fn [m _ctx]
    (-> m keys first namespace keyword)))

(defmethod prepare :default [m _] m)

(defmethod prepare :entity
  [entity ctx]
  (update-in entity [:entity/user] #(find-user ctx %)))

(defmethod prepare :commodity
  [commodity ctx]
  (-> commodity
      (update-in [:commodity/entity] #(find-entity ctx %))
      (update-in [:commodity/price-config] (fnil identity {:price-config/enabled true}))))

(defmethod prepare :price
  [price ctx]
  (update-in price [:price/commodity] #(find-commodity ctx %)))

(defn- resolve-account-commodity
  [{:as account :account/keys [entity]} ctx]
  (update-in account
             [:account/commodity]
             #(if %
                (find-commodity ctx %)
                (find ctx (fn [{:commodity/keys [type] :as m}]
                            (and = (:currency type)
                                 (= entity (:commodity/entity m))))))))

(defmethod prepare :account
  [account ctx]
  {:pre [(:account/entity account)]}
  (-> account
      (update-in [:account/entity] #(find-entity ctx %))
      (resolve-account-commodity ctx)
      (update-in-if [:account/parent] #(find-account ctx %))))

(defmethod prepare :transaction
  [trx ctx]
  (-> trx
      expand
      (update-in [:transaction/items] (fn [i] (mapv #(prepare % ctx) i)))
      (update-in [:transaction/entity] #(find-entity ctx %))))

(defmethod prepare :transaction-item
  [item ctx]
  {:pre [(:transaction-item/account item)]}

  (update-in item
             [:transaction-item/account]
             #(find-account ctx %)))

(defmethod prepare :reconciliation
  [recon ctx]
  (let [account (find-account ctx (:reconciliation/account recon))]
    (-> recon
        (assoc :reconciliation/account account)
        (update-in [:reconciliation/item-refs]
                   (fn [i]
                     (mapv (comp (juxt :id :transaction-item/transaction-date)
                                 #(find-transaction-item ctx %)
                                 #(conj % account))
                           i))))))

(defmethod prepare :budget
  [budget ctx]
  (-> budget
      (update-in [:budget/entity] #(find-entity ctx %))
      (update-in [:budget/items] (fn [i] (map #(prepare % ctx) i)))))

(defmethod prepare :budget-item
  [item ctx]
  (update-in item [:budget-item/account] #(find-account ctx %)))

(defmethod prepare :image
  [image ctx]
  (-> image
      (update-in [:image/user] #(find-user ctx %))
      (update-in [:image/body] (comp read-bytes
                                     io/input-stream))))

(defmethod prepare :attachment
  [att ctx]
  (-> att
      (update-in [:attachment/transaction] #(find-transaction ctx %))
      (update-in [:attachment/image] #(find-image ctx %))))

(defn realize
  "Realizes a test context"
  [input]
  (reduce (fn [ctx m]
            (conj ctx
                  (-> m
                      (prepare ctx)
                      models/put)))
          []
          input))

(defmacro with-context
  [& args]
  (let [[context body] (if (symbol? (first args))
                         [(first args) (rest args)]
                         [basic-context args])]
    `(binding [*context* (realize ~context)]
       ~@body)))
