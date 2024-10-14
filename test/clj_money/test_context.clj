(ns clj-money.test-context
  (:refer-clojure :exclude [find])
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.factories.user-factory]
            [clj-money.io :refer [read-bytes]]
            [clj-money.models :as models]
            [clj-money.models.grants :as grants]
            [clj-money.models.cached-prices :as cached-prices]
            [clj-money.models.lots :as lots]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.scheduled-transactions :as sched-trans]
            [clj-money.models.attachments :as attachments]
            [clj-money.models.reconciliations :as reconciliations]
            [clj-money.models.images :as images]
            [clj-money.models.imports :as imports]
            [clj-money.models.identities :as idents]
            [clj-money.trading :as trading]
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
            :user-id "john@doe.com"}
   #:entity{:name "Business"
            :user-id "jane@doe.com"}
   #:commodity{:name "US Dollar"
               :entity-id "Personal"
               :symbol "USD"
               :type :currency}
   #:commodity{:name "US Dollar"
               :entity-id "Business"
               :symbol "USD"
               :type :currency}
   #:account{:name "Opening Balances"
             :type :equity
             :entity-id "Personal"}
   #:account{:name "Checking"
             :entity-id "Personal"
             :type :asset}
   #:account{:name "Salary"
             :entity-id "Personal"
             :type :income}
   #:account{:name "Rent"
             :entity-id "Personal"
             :type :expense}
   #:account{:name "Groceries"
             :entity-id "Personal"
             :type :expense}
   #:account{:name "Tax"
             :entity-id "Personal"
             :type :expense}
   #:account{:name "FIT"
             :type :expense
             :entity-id "Personal"
             :parent-id "Tax"}
   #:account{:name "Medicare"
             :type :expense
             :entity-id "Personal"
             :parent-id "Tax"}
   #:account{:name "Sales"
             :entity-id "Business"
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
   {:pre [k v]}
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
  ([transaction-date description] (find-transaction *context* transaction-date description))
  ([context transaction-date description]
   {:pre [(string? description) (t/local-date? transaction-date)]}

   (find context
         :transaction/transaction-date transaction-date
         :transaction/description description)))

(defn find-transaction-item
  ([transaction-date quantity account-id]
   (find-transaction-item *context*
                          transaction-date
                          quantity
                          account-id))
  ([context transaction-date quantity account-id]
   (->> context
        (filter #(= transaction-date (:transaction-date %)))
        (mapcat :items)
        (filter #(and (= account-id (:account-id %))
                      (= quantity (:quantity %))))
        first)))

(defn find-scheduled-transaction
  ([description] (find-scheduled-transaction *context* description))
  ([context description]
   (find context :scheduled-transaction/description description)))

(defn find-recon
  ([account-name end-of-period]
   (find-recon *context*
               account-name
               end-of-period))
  ([ctx account-name end-of-period]
   (let [account (find-account ctx account-name)]
     (find ctx
           :reconciliation/account account
           :reconciliation/end-of-period end-of-period))))

(defn- realize-cached-prices
  [context]
  (update-in context [:cached-prices] #(mapv cached-prices/create %)))

(defn- resolve-user
  [model context k]
  (update-in model [k] #(find-user context %)))

(defn- resolve-entity
  [model context k]
  (update-in model [k] (fn [entity-name]
                         (if entity-name
                           (find-entity context entity-name)
                           (-> context :entities first)))))

(defn create-grants
  [context grants]
  (mapv (fn [attributes]
          (grants/create (-> attributes
                             (resolve-user context :grant/user)
                             (resolve-entity context :grant/entity))))
        grants))

(defn- realize-grants
  [context]
  (update-in context [:grants] #(create-grants context %)))

(defn- resolve-parent
  [{:account/keys [parent entity] :as account}]
  (if parent
    (assoc account :account/parent (models/find-by {:account/name parent
                                                    :account/entity entity}))
    account))

(defn- resolve-commodity
  [model context k]
  (update-in model [k] (fn [symbol]
                         (or (if symbol
                               (find-commodity context symbol)
                               (->> (:commodities context)
                                    (filter #(and (= :currency (:commodity/type %))
                                                  (= (get-in % [:commodity/entity :id])
                                                     (get-in model [:account/entity :id]))))
                                    first))
                             (throw (ex-info "Could not resolve commodity" model))))))

(defn- resolve-account
  ([model context] (resolve-account model context :account-id))
  ([model context k]
   (update-in model [k] #(:id (find-account context %)))))

(defn- coerce-quantity
  [item]
  (update-in item [:quantity] bigdec))

(defn- prepare-item
  [context item]
  (-> item
      (resolve-account context)
      coerce-quantity))

(defn- prepare-items
  [transaction context]
  (update-in transaction [:items] (fn [items]
                                    (map #(prepare-item context %)
                                         items))))

(defn- create-transaction
  [transaction context]
  (-> transaction
      (resolve-entity context :transaction/entity)
      expand
      (prepare-items context)
      transactions/create))

(defn- create-transactions
  [context transactions]
  (mapv #(create-transaction % context) transactions))

(defn- realize-transactions
  [context]
  (update-in context [:transactions] #(create-transactions context %)))

(defn- create-scheduled-transaction
  [sched-tran context]
  (-> sched-tran
      (resolve-entity context :scheduled-transaction/entity)
      (prepare-items context)
      sched-trans/create))

(defn- create-scheduled-transactions
  [context sched-trans]
  (mapv #(create-scheduled-transaction % context) sched-trans))

(defn- realize-scheduled-transactions
  [context]
  (update-in context [:scheduled-transactions] #(create-scheduled-transactions context %)))

(defn- resolve-transaction
  [model context]
  (let [trans-ref (:transaction-id model)]
    (assoc model :transaction-id (->> context
                                      :transactions
                                      (filter #(and
                                                (= (:transaction-date %)
                                                   (:transaction-date trans-ref))
                                                (= (:description %)
                                                   (:description trans-ref))))
                                      (map (juxt :id :transaction-date))
                                      first))))

(defn- resolve-image
  [model context]
  (update-in model [:image-id] #(:id (find-image context %))))

(defn- resolve-images
  [model context]
  (update-in model [:image-ids] (fn [file-names]
                                  (map (comp :id #(find-image context %))
                                       file-names))))

(defn- rearrange-transaction-attributes
  [attachment]
  (assoc attachment :transaction-id (-> attachment :transaction-id first)
         :transaction-date (-> attachment :transaction-id second)))

(defn- create-attachments
  [context attachments]
  (mapv #(-> %
             (resolve-transaction context)
             rearrange-transaction-attributes
             (resolve-image context)
             attachments/create)
        attachments))

(defn- realize-attachments
  [context]
  (update-in context [:attachments] #(create-attachments context %)))

(defn- resolve-budget-items
  [budget context]
  (update-in budget
             [:items]
             (fn [items]
               (map #(resolve-account % context) items))))

(defn- create-budgets
  [context budgets]
  (mapv (fn [attributes]
          (-> attributes
              (resolve-entity context :budget/entity)
              (resolve-budget-items context)
              budgets/create))
        budgets))

(defn- realize-budgets
  [context]
  (update-in context [:budgets] #(create-budgets context %)))

(defn- create-lots
  [context lots]
  (mapv (fn [attributes]
          (-> attributes
              (resolve-commodity context :lot/commodity)
              (resolve-account context)
              (lots/create)))
        lots))

(defn- realize-lots
  [context]
  (update-in context [:lots] #(create-lots context %)))

(defn- resolve-item-ref
  [{:keys [transaction-date quantity]} context account-id]
  (or (->> (:transactions context)
           (filter #(= transaction-date (:transaction-date %)))
           (mapcat :items)
           (filter #(and (= account-id (:account-id %))
                         (= quantity (:quantity %))))
           (map (juxt :id :transaction-date))
           first)
      (throw (Exception. (format "Unable to find a transaction with date=%s, quantity=%s"
                                 transaction-date
                                 quantity)))))

(defn- resolve-item-refs
  [model context]
  (update-in model
             [:item-refs]
             (fn [item-refs]
               (mapv #(resolve-item-ref % context (:account-id model))
                     item-refs))))

(defn- create-reconciliations
  [context reconciliations]
  (mapv (fn [attributes]
          (-> attributes
              (resolve-account context)
              (resolve-item-refs context)
              reconciliations/create))
        reconciliations))

(defn- realize-reconciliations
  [context]
  (update-in context [:reconciliations] #(create-reconciliations context %)))

(defn- read-file
  [image]
  (update-in image [:body] #(read-bytes %)))

(defn- create-image-from-file
  [model]
  (images/create (read-file model)))

(defn- create-images
  [context images]
  (mapv (fn [attributes]
          (-> attributes
              (resolve-user context :image/user)
              create-image-from-file))
        images))

(defn- realize-images
  [context]
  (update-in context [:images] #(create-images context %)))

(defn- create-imports
  [context imports]
  (mapv (fn [attributes]
          (-> attributes
              (resolve-user context :import/user)
              (resolve-images context)
              imports/create))
        imports))

(defn- realize-imports
  [context]
  (update-in context [:imports] #(create-imports context %)))

(defn- execute-trade
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

(defn- execute-trades
  [trades context]
  (mapv #(execute-trade % context) trades))

(defn- realize-trades
  [context]
  (update-in context [:trades] execute-trades context))

(defn- create-identities
  [idents context]
  {:pre [(sequential? idents)]}

  (mapv #(idents/create (resolve-user % context :identity/user))
        idents))

(defn- realize-identities
  [context]
  (update-in context [:identities] (fnil create-identities []) context))

(defn- extract-monitored-account-ids
  [entities]
  (->> entities
       (map (fn [{{:keys [monitored-account-ids]} :settings :keys [name]}]
              (when monitored-account-ids
                [name monitored-account-ids])))
       (filter identity)
       (into {})))

(defn- stash-monitored-account-ids
  [{:keys [entities] :as context}]
  (-> context
      (assoc :monitored-account-ids (extract-monitored-account-ids entities))
      (assoc :entities (map #(update-in % [:entity/settings] dissoc :monitored-account-ids)
                            entities))))

(defn- apply-monitored-account-ids
  [entity {:keys [monitored-account-ids] :as context}]
  (if-let [account-ids (get-in monitored-account-ids [(:name entity)])]
    (models/put (assoc-in entity [:entity/settings :settings/monitored-account-ids]
                          (->> account-ids
                               (map (comp :id
                                          #(find-account context %)))
                               set)))
    entity))

(defn- update-monitored-account-ids
  [context]
  (-> context
      (update-in [:entities]
                 (fn [entities]
                   (map #(apply-monitored-account-ids % context)
                        entities)))
      (dissoc :monitored-account-ids)))

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
      (update-in [:transaction/items]
                 (fn [items]
                   (map (fn [item]
                          (-> item
                              (update-in [:transaction-item/account]
                                         #(find-account ctx %))))
                        items)))
      expand
      (update-in [:transaction/entity] #(find-entity ctx %))))

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
