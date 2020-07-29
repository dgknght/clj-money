(ns clj-money.test-context
  (:require [stowaway.core :refer [with-storage]]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.io :refer [read-bytes]]
            [clj-money.validation :as validation]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.grants :as grants]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]
            [clj-money.models.lots :as lots]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.attachments :as attachments]
            [clj-money.models.reconciliations :as reconciliations]
            [clj-money.models.images :as images]
            [clj-money.models.imports :as imports]
            [clj-money.models.identities :as idents]
            [clj-money.trading :as trading])
  (:import org.joda.time.LocalDate))

(def basic-context
  {:users (->> ["john@doe.com" "jane@doe.com"]
               (mapv #(factory :user {:email %})))
   :entities [{:name "Personal"
               :user-id "john@doe.com"}
              {:name "Business"
               :user-id "jane@doe.com"}]
   :commodities [{:name "US Dollar"
                  :entity-id "Personal"
                  :symbol "USD"
                  :type :currency}
                 {:name "US Dollar"
                  :entity-id "Business"
                  :symbol "USD"
                  :type :currency}]
   :accounts [{:name "Checking"
               :entity-id "Personal"
               :type :asset}
              {:name "Salary"
               :entity-id "Personal"
               :type :income}
              {:name "Rent"
               :entity-id "Personal"
               :type :expense}
              {:name "Groceries"
               :entity-id "Personal"
               :type :expense}
              {:name "Sales"
               :entity-id "Business"
               :type :income}]})

(defn- find-in-context
  [context model-group-key model-id-key model-id]
  (->> context
       model-group-key
       (filter #(= model-id (model-id-key %)))
       first))

(defn find-user
  [context email]
  (find-in-context context :users :email email))

(defn find-users
  [context & emails]
  (map #(find-user context %) emails))

(defn find-entity
  [context entity-name]
  (find-in-context context :entities :name entity-name))

(defn find-entities
  [context & entity-names]
  (map #(find-entity context %) entity-names))

(defn find-import
  [context entity-name]
  (find-in-context context :imports :entity-name entity-name))

(defn find-imports
  [context & entity-names]
  (map #(find-import context %) entity-names))

(defn find-grant
  [context entity-id user-id]
  (->> context
       :grants
       (filter #(and (= entity-id (:entity-id %))
                     (= user-id (:user-id %))))
       first))

(defn find-account
  [context account-name]
  (find-in-context context :accounts :name account-name))

(defn find-accounts
  [context & account-names]
  (map #(find-account context %) account-names))

(defn find-attachment
  [context caption]
  (find-in-context context :attachments :caption caption))

(defn find-image
  [context original-filename]
  (find-in-context context :images :original-filename original-filename))

(defn find-commodity
  [context symbol]
  (find-in-context context :commodities :symbol symbol))

(defn find-commodities
  [context & symbols]
  (map #(find-commodity context %) symbols))

(defn find-budget
  [context budget-name]
  (find-in-context context :budgets :name budget-name))

(defn find-price
  [context sym trade-date]
  (let [commodity (find-commodity context sym)]
    (->> context
         :prices
         (filter #(and (= (:id commodity) (:commodity-id %))
                       (= trade-date (:trade-date %))))
         first)))

(defn find-transaction
  [context transaction-date description]
  {:pre [(string? description) (instance? LocalDate transaction-date)]}

  (->> (:transactions context)
       (filter #(and (= transaction-date (:transaction-date %))
                     (= description (:description %))))
       first))

(defn find-transaction-item
  [context transaction-date quantity account-id]
  (->> (:transactions context)
       (filter #(= transaction-date (:transaction-date %)))
       (mapcat :items)
       (filter #(and (= account-id (:account-id %))
                     (= quantity (:quantity %))))
       first))

(defn find-recon
  [{:keys [reconciliations] :as ctx} account-name end-of-period]
  (let [account (find-account ctx account-name)]
    (->> reconciliations
         (filter #(and (= (:id account) (:account-id %))
                       (= end-of-period (:end-of-period %))))
         first)))

(defn- throw-on-invalid
  [model]
  (if (validation/has-error? model)
    (throw (ex-info (format "Unable to create the model. %s"
                            (prn-str model))
                    model))
    model))

(defn- create-users
  [storage users]
  (mapv (fn [attributes]
         (throw-on-invalid (users/create storage attributes)))
       users))

(defn- realize-users
  [context storage]
  (update-in context [:users] #(create-users storage %)))

(defn- resolve-user
  [model context]
  (assoc model :user-id (:id (if-let [id (:user-id model)]
                               (find-user context id)
                               (-> context :users first)))))

(defn- create-entities
  [entities storage context]
  (mapv (fn [attributes]
          (if (:id attributes)
            attributes
            (throw-on-invalid
              (entities/create storage (resolve-user attributes context)))))
        entities))

(defn- realize-entities
  [context storage]
  (update-in context [:entities] create-entities storage context))

(defn- resolve-entity
  [model context]
  (update-in model [:entity-id] (fn [entity-name]
                                  (if entity-name
                                    (:id (find-entity context entity-name))
                                    (-> context :entities first :id)))))

(defn create-grants
  [storage context grants]
  (mapv (fn [attributes]
          (grants/create storage (-> attributes
                                     (resolve-user context)
                                     (resolve-entity context))))
        grants))

(defn- realize-grants
  [context storage]
  (update-in context [:grants] #(create-grants storage context %)))

(defn- resolve-parent
  [account storage]
  (if (:parent-id account)
    (let [parent (accounts/find-by-name storage (:entity-id account) (:parent-id account))]
      (assoc account :parent-id (:id parent)))
    account))

(defn- resolve-commodity
  [model context]
  (update-in model [:commodity-id] (fn [symbol]
                                     (if symbol
                                       (:id (find-commodity context symbol))
                                       (->> (:commodities context)
                                            (filter #(= :currecy (:type %)))
                                            first)))))

(defn- create-account
  [storage context attributes]
  (if (:id attributes)
    attributes
    (accounts/create storage (-> attributes
                                 (resolve-entity context)
                                 (resolve-commodity context)
                                 (resolve-parent storage)))))

(defn- create-accounts
  "Creates the specified accounts.
  
  Accounts can be a sequence of maps containing account properties,
  or a map where the keys are account types and the values
  are vectors of account properties"
  [storage context accounts]
  (let [account-list (if (map? accounts)
                       (mapcat (fn [[account-type acct-list]]
                                 (map #(assoc % :type account-type)
                                      acct-list))
                               accounts)
                       accounts)]
    (mapv #(create-account storage context %) account-list)))

(defn- realize-accounts
  [context storage]
  (update-in context [:accounts] #(create-accounts storage context %)))

(defn- resolve-account
  [model context]
  (update-in model [:account-id] #(:id (find-account context %))))

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

(defn- expand-items
  [transaction]
  (if (:items transaction)
    transaction
    (-> transaction
        (assoc :items [{:action :debit
                        :quantity (:quantity transaction)
                        :account-id (:debit-account-id transaction)}
                       {:action :credit
                        :quantity (:quantity transaction)
                        :account-id (:credit-account-id transaction)}])
        (dissoc :quantity :debit-account-id :credit-account-id))))

(defn- create-transaction
  [transaction storage context]
  (transactions/create storage (-> transaction
                                   (resolve-entity context)
                                   expand-items
                                   (prepare-items context))))

(defn- create-transactions
  [storage context transactions]
  (mapv #(create-transaction % storage context) transactions))

(defn- realize-transactions
  [context storage]
  (update-in context [:transactions] #(create-transactions storage context %)))

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

(defn- create-attachment
  [model storage]
  (attachments/create storage model))

(defn- rearrange-transaction-attributes
  [attachment]
  (assoc attachment :transaction-id (-> attachment :transaction-id first)
                    :transaction-date (-> attachment :transaction-id second)))

(defn- create-attachments
  [storage context attachments]
  (mapv #(-> %
             (resolve-transaction context)
             rearrange-transaction-attributes
             (resolve-image context)
             (create-attachment storage)
             throw-on-invalid)
        attachments))

(defn- realize-attachments
  [context storage]
  (update-in context [:attachments] #(create-attachments storage context %)))

(defn- resolve-budget-items
  [budget context]
  (update-in budget
             [:items]
             (fn [items]
               (map #(resolve-account % context) items))))

(defn- create-budget
  [model storage]
  (budgets/create storage model))

(defn- create-budgets
  [storage context budgets]
  (mapv (fn [attributes]
          (-> attributes
              (resolve-entity context)
              (resolve-budget-items context)
              (create-budget storage)))
        budgets))

(defn- realize-budgets
  [context storage]
  (update-in context [:budgets] #(create-budgets storage context %)))

(defn- create-commodity
  [model storage]
  (commodities/create storage model))

(defn- create-commodities
  [storage context commodities]
  (mapv (fn [attributes]
          (-> attributes
              (resolve-entity context)
              (create-commodity storage)
              throw-on-invalid))
        commodities))

(defn- realize-commodities
  [context storage]
  (update-in context [:commodities] #(create-commodities storage context %)))

(defn- create-price
  [model storage]
  (prices/create storage model))

(defn- create-prices
  [storage context prices]
  (mapv (fn [attributes]
          (-> attributes
              (resolve-commodity context)
              (create-price storage)))
        prices))

(defn- realize-prices
  [context storage]
  (update-in context [:prices] #(create-prices storage context %)))

(defn- create-lot
  [model storage]
  (lots/create storage model))

(defn- create-lots
  [storage context lots]
  (mapv (fn [attributes]
          (-> attributes
              (resolve-commodity context)
              (resolve-account context)
              (create-lot storage)))
        lots))

(defn- realize-lots
  [context storage]
  (update-in context [:lots] #(create-lots storage context %)))

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

(defn- create-reconciliation
  [model storage]
  (reconciliations/create storage model))

(defn- create-reconciliations
  [storage context reconciliations]
  (mapv (fn [attributes]
          (-> attributes
              (resolve-account context)
              (resolve-item-refs context)
              (create-reconciliation storage)
              throw-on-invalid))
        reconciliations))

(defn- realize-reconciliations
  [context storage]
  (update-in context [:reconciliations] #(create-reconciliations storage context %)))

(defn- read-file
  [image]
  (update-in image [:body] #(read-bytes %)))

(defn- create-image-from-file
  [model storage]
  (images/create storage (read-file model)))

(defn- create-images
  [storage context images]
  (mapv (fn [attributes]
          (-> attributes
              (resolve-user context)
              (create-image-from-file storage)
              throw-on-invalid))
        images))

(defn- realize-images
  [context storage]
  (update-in context [:images] #(create-images storage context %)))

(defn- create-import
  [model storage]
  (imports/create storage model))

(defn- create-imports
  [storage context imports]
  (mapv (fn [attributes]
          (-> attributes
              (resolve-user context)
              (resolve-images context)
              (create-import storage)
              throw-on-invalid))
        imports))

(defn- realize-imports
  [context storage]
  (update-in context [:imports] #(create-imports storage context %)))

(defn- execute-trade
  [trade storage context]
  (let [f (case (:type trade)
            :purchase (partial trading/buy storage)
            :sale     (partial trading/sell storage))]
    (-> trade
        (resolve-entity context)
        (resolve-account context)
        (resolve-commodity context)
        f)))

(defn- execute-trades
  [trades storage context]
  (mapv #(execute-trade % storage context) trades))

(defn- realize-trades
  [context storage]
  (update-in context [:trades] execute-trades storage context))

(defn- create-identities
  [idents storage context]
  {:pre [(sequential? idents)]}

  (mapv #(idents/create storage (resolve-user % context))
        idents))

(defn- realize-identities
  [context storage]
  (update-in context [:identities] (fnil create-identities []) storage context))

(defn- extract-monitored-account-ids
  [entities]
  (->> entities
       (map (fn [{{:keys [monitored-account-ids]} :settings :keys [name]}]
              (when monitored-account-ids
                [name monitored-account-ids])))
       (filter identity)
       (into {})))

(defn- stash-monitored-account-ids
  [{:keys [entities] :as context} _]
  (-> context
      (assoc :monitored-account-ids (extract-monitored-account-ids entities))
      (assoc :entities (map #(update-in % [:settings] dissoc :monitored-account-ids)
                            entities))))

(defn- apply-monitored-account-ids
  [entity {:keys [monitored-account-ids] :as context} storage]
  (if-let [account-ids (get-in monitored-account-ids[(:name entity)])]
    (entities/update storage (assoc-in entity [:settings :monitored-account-ids]
                                       (->> account-ids
                                            (map (comp :id
                                                       #(find-account context %)))
                                            set)))
    entity))

(defn- update-monitored-account-ids
  [context storage]
  (-> context
      (update-in [:entities]
                 (fn [entities]
                   (map #(apply-monitored-account-ids % context storage)
                        entities)))
      (dissoc :monitored-account-ids)))

(defn realize
  "Realizes a test context"
  [storage-spec input]
  (with-storage [s storage-spec]
    (-> input
        (realize-users s)
        (realize-images s)
        (realize-imports s)
        (stash-monitored-account-ids s)
        (realize-entities s)
        (realize-grants s)
        (realize-commodities s)
        (realize-accounts s)
        (update-monitored-account-ids s)
        (realize-prices s)
        (realize-lots s)
        (realize-budgets s)
        (realize-transactions s)
        (realize-trades s)
        (realize-attachments s)
        (realize-reconciliations s)
        (realize-identities s))))
