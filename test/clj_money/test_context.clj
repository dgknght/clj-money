(ns clj-money.test-context
  (:refer-clojure :exclude [find])
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clojure.java.io :as io]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.io :refer [read-bytes]]
            [clj-money.util :as util :refer [entity=]]
            [clj-money.entities :as entities]
            [clj-money.images :as images]
            [clj-money.entities.propagation :as prop]
            [clj-money.trading :as trading]))

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
   {:pre [context
          (sequential? context)
          k
          v]}
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
  ([arg]
   (if (sequential? arg)
     (partial find-user arg)
     (find-user *context* arg)))
  ([context email]
   (find context :user/email email)))

(defn- context+
  [args]
  (if (map? (first args))
    args
    (cons *context* args)))

(defn find-entity
  ([arg]
   (if (string? arg)
     (find-entity *context* arg)
     (partial find-entity arg)))
  ([context entity-name]
   {:pre [context entity-name]}
   (find context :entity/name entity-name)))

(defn find-import
  ([entity-name] (find-import *context* entity-name))
  ([context entity-name]
   (find context :import/entity-name entity-name)))

(defn find-grant
  ([identifier] (find-grant *context* identifier))
  ([context [entity user]]
   (let [e (util/->entity-ref
             (if (map? entity)
               entity
               (find-entity context entity)))
         u (util/->entity-ref
             (if (map? user)
               user
               (find-user context user)))]
     (find context :grant/entity e :grant/user u))))

(defn find-account
  ([arg]
   (if (string? arg)
     (find-account *context* arg)
     (partial find-account arg)))
  ([context account-name]
   {:pre [(coll? context) (string? account-name)]}
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
  ([arg]
   (if (string? arg)
     (find-image *context* arg)
     (partial find-image arg)))
  ([context original-filename]
   (find context :image/original-filename original-filename)))

(defn find-commodity
  ([arg]
   (if (string? arg)
     (find-commodity *context* arg)
     (partial find-commodity arg)))
  ([context symbol]
   (find context :commodity/symbol symbol)))

(defn find-budget
  ([budget-name] (find-budget *context* budget-name))
  ([context budget-name]
   (find context :budget/name budget-name)))

(defn find-budget-item
  ([identifier] (find-budget-item *context* identifier))
  ([context [budget-name account-name]]
   (let [budget (find-budget context budget-name)
         account (find-account context account-name)]
     (->> (:budget/items budget)
          (filter #(util/id= account (:budget-item/account %)))
          first))))

(defn find-price
  ([identifier] (find-price *context* identifier))
  ([context [sym trade-date]]
   (let [commodity (find-commodity context sym)]
     (find context #(and (= (:id commodity)
                            (get-in % [:price/commodity :id]))
                         (= trade-date
                            (:price/trade-date %)))))))

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
  ([context [transaction-date value]]
   (->> context
        (filter #(= transaction-date (:transaction/transaction-date %)))
        (mapcat :transaction/items)
        (filter #(= value (:transaction-item/value %)))
        (map #(assoc % :transaction/transaction-date transaction-date))
        first)))

(defmacro defind
  [fn-name & body]
  `(defn ~fn-name
     ([identifier#]
      (~fn-name *context* identifier#))
     (~@body)))

#_:clj-kondo/ignore
(defind find-account-item
  [context [transaction-date value account :as id]]
  (let [act (if (map? account)
              account
              (find-account context account))]
    (->> context
         (filter #(= transaction-date (:transaction/transaction-date %)))
         (mapcat :transaction/items)
         (filter #(= value (:transaction-item/value %)))
         (mapcat (juxt :transaction-item/debit-item
                       :transaction-item/credit-item))
         (filter #(entity= act (:account-item/account %)))
         (map #(assoc % :transaction/transaction-date transaction-date))
         first)))

(defn find-scheduled-transaction
  ([description] (find-scheduled-transaction *context* description))
  ([context description]
   (find context :scheduled-transaction/description description)))

(defn find-reconciliation
  ([identifier]
   (find-reconciliation *context* identifier))
  ([ctx [account end-of-period]]
   (let [act (util/->entity-ref (if (map? account)
                                 account
                                 (find-account ctx account)))]
     (find ctx
           :reconciliation/account act
           :reconciliation/end-of-period end-of-period))))

(defn find-lot
  ([identifier] (find-lot *context* identifier))
  ([ctx [account commodity]]
   (let [act (util/->entity-ref (if (map? account)
                                 account
                                 (find-account ctx account)))
         cmd (util/->entity-ref (if (map? commodity)
                                 commodity
                                 (find-commodity ctx commodity)))]
     (find ctx
           :lot/account act
           :lot/commodity cmd))))

(defmulti ^:private prepare
  (fn [m _ctx]
    (-> m keys first namespace keyword)))

(defmethod prepare :default [m _] m)

(defmethod prepare :entity
  [entity ctx]
  (update-in entity [:entity/user] (find-user ctx)))

(defmethod prepare :commodity
  [commodity ctx]
  (-> commodity
      (update-in [:commodity/entity] (find-entity ctx))
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
      (update-in [:account/entity] (find-entity ctx))
      (resolve-account-commodity ctx)
      (update-in-if [:account/parent] #(find-account ctx %))))

(defn- prepare-coll
  [ctx]
  (fn [items]
    (mapv #(prepare % ctx) items)))

(defmethod prepare :transaction
  [trx ctx]
  (-> trx
      (update-in [:transaction/entity] (find-entity ctx))
      (update-in-if [:transaction/credit-account] (find-account ctx))
      (update-in-if [:transaction/debit-account] (find-account ctx))
      (update-in-if [:transaction/items] (prepare-coll ctx))))

(defmethod prepare :scheduled-transaction
  [trx ctx]
  (-> trx
      (update-in [:scheduled-transaction/items] (prepare-coll ctx))
      (update-in [:scheduled-transaction/entity] (find-entity ctx))))

(defmethod prepare :transaction-item
  [item ctx]
  (-> item
      (update-in-if [:transaction-item/account] (find-account ctx))
      (update-in-if [:transaction-item/debit-item
                     :account-item/account] (find-account ctx))
      (update-in-if [:transaction-item/credit-item
                     :account-item/account] (find-account ctx))))

(defmethod prepare :scheduled-transaction-item
  [item ctx]
  {:pre [(:scheduled-transaction-item/account item)]}

  (update-in item
             [:scheduled-transaction-item/account]
             (find-account ctx)))

(defmethod prepare :reconciliation
  [recon ctx]
  (let [account (find-account ctx (:reconciliation/account recon))]
    (-> recon
        (assoc :reconciliation/account account)
        (update-in [:reconciliation/items]
                   (partial mapv (comp #(find-account-item ctx %)
                                       #(conj % account)))))))

(defmethod prepare :budget
  [budget ctx]
  (-> budget
      (update-in [:budget/entity] (find-entity ctx))
      (update-in [:budget/items] (partial map #(prepare % ctx)))))

(defmethod prepare :budget-item
  [item ctx]
  (-> item
      (update-in-if [:budget-item/periods] vec)
      (update-in [:budget-item/account] #(find-account ctx %))))

(defmethod prepare :image
  [image ctx]
  (-> image
      (update-in [:image/user] (find-user ctx))
      (assoc :image/uuid (-> (:image/content image)
                             io/input-stream
                             read-bytes
                             images/put))
      (dissoc :image/content)))

(defmethod prepare :attachment
  [att ctx]
  (-> att
      (update-in [:attachment/transaction] #(find-transaction ctx %))
      (update-in [:attachment/image] (find-image ctx))))

(defmethod prepare :grant
  [attr ctx]
  (-> attr
      (update-in [:grant/user] (find-user ctx))
      (update-in [:grant/entity] (find-entity ctx))))

(defmethod prepare :identity
  [attr ctx]
  (update-in attr [:identity/user] (find-user ctx)))

(defmethod prepare :import
  [attr ctx]
  (-> attr
      (update-in [:import/user] (find-user ctx))
      (update-in [:import/images] (fn [img-refs]
                                    (mapv (comp util/->entity-ref
                                                (find-image ctx))
                                          img-refs)))))

(defmethod prepare :lot
  [attr ctx]
  (-> attr
      (update-in [:lot/account] (find-account ctx))
      (update-in [:lot/commodity] (find-commodity ctx))))

(defn- resolve-trade-accounts
  [attr ctx]
  (let [find-act (find-account ctx)]
    (reduce (fn [m k]
              (update-in-if m [k] find-act))
            (update-in-if attr [:trade/commodity] (find-commodity ctx))
            [:trade/account
             :trade/commodity-account
             :trade/lt-capital-gains-account
             :trade/lt-capital-loss-account
             :trade/st-capital-gains-account
             :trade/st-capital-loss-account])))

(defmethod prepare :trade
  [attr ctx]
  (-> attr
      (update-in [:trade/entity] (find-entity ctx))
      (resolve-trade-accounts ctx)))

(def ^:private extract-purchase-entities
  (comp flatten
        (juxt :trade/transactions
              :trade/commodity-account
              :trade/lot)))

(defn- buy
  [trade]
  (extract-purchase-entities (trading/buy trade)))

(def ^:private extract-sale-entities
  (comp flatten (juxt :trade/transactions)))

(defn- sell
  [trade]
  (extract-sale-entities (trading/sell trade)))

(defn- process
  [m]
  (case (util/entity-type m)
    :trade (do
             (assert (#{:sale :purchase} (:trade/type m)))
             (if (= :purchase (:trade/type m))
               (buy m)
               (sell m)))
    [(entities/put m)]))

(defmulti post-process util/entity-type)

(defmethod post-process :default [m] m)

(defmethod post-process :entity
  [entity]
  (if (get-in entity [:entity/settings :settings/default-commodity])
    entity
    (if-let [commodity (entities/find-by {:commodity/entity entity
                                          :commodity/type :currency})]
      (-> entity
          entities/find
          (assoc-in [:entity/settings
                     :settings/default-commodity]
                    (util/->entity-ref commodity))
          entities/put)
      entity)))

(defn realize
  "Realizes a test context"
  [input]
  (let [result (->> input
                    (reduce (fn [ctx m]
                              (apply conj
                                     ctx
                                     (-> m
                                         (prepare ctx)
                                         process)))
                            [])
                    (mapv post-process))]
    (prop/propagate-all {})
    result))

(defmacro with-context
  [& args]
  (let [[context body] (if (symbol? (first args))
                         [(first args) (rest args)]
                         [basic-context args])]
    `(binding [*context* (realize ~context)]
       ~@body)))
