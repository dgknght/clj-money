(ns clj-money.models.accounts
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clojure.string :as string]
            [clojure.set :refer [rename-keys]]
            [clj-money.util :refer [pprint-and-return
                                    safe-read-string]]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.authorization :as authorization]
            [clj-money.shared :refer [user-owns-entity?]]
            [clj-money.models.helpers :refer [with-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [create-account
                                              find-account-by-id
                                              find-account-by-entity-id-and-name
                                              select-accounts
                                              update-account
                                              delete-account]]
            [clj-money.models.entities :as entities]
            [clj-money.models.commodities :as commodities])
  (:import java.math.BigDecimal))

(def account-types
  "The list of valid account types in standard presentation order"
  [:asset :liability :equity :income :expense])

(s/def ::id integer?)
(s/def ::entity-id integer?)
(s/def ::name validation/non-empty-string?)
(s/def ::type #{:asset :liability :equity :income :expense})
(s/def ::commodity-id integer?)
(s/def ::parent-id (s/nilable integer?))
(s/def ::new-account (s/keys :req-un [::entity-id ::name ::type ::commodity-id] :opt-un [::parent-id]))
(s/def ::existing-account (s/keys :req-un [::id ::entity-id ::type ::name] :opt-un [::parent-id ::commodity-id]))
; :balance and :children-balance are not specified because they are always calculated and not passed in

(def ^:private coercion-rules
  [(coercion/rule :integer [:id])
   (coercion/rule :keyword [:type])
   (coercion/rule :integer [:commodity-id])
   (coercion/rule :integer [:entity-id])
   (coercion/rule :integer [:parent-id])])

(defn- default-commodity-id
  [storage entity-id]
  (let [entity (entities/find-by-id storage entity-id)]
    (or (-> entity :settings :default-commodity-id)
        (->> {:entity-id entity-id
              :type :currency}
             (commodities/search storage)
             first
             :id))))

(declare find-by-id)
(defn- before-validation
  "Adjust account data for validation"
  [storage account]
  (cond-> account
    ; If no entity is specified, try to look it up
    (and (:id account)
         (nil? (:entity-id account)))
    (assoc :entity-id (:entity-id (find-by-id storage (:id account))))

    ; strip out empty string for parent-id
    (and (string? (:parent-id account))
         (empty? (:parent-id account)))
    (dissoc :parent-id)

    ; if no commodity is specified, use the default
    (nil? (:commodity-id account))
    (assoc :commodity-id (default-commodity-id storage (:entity-id account)))))

(defn- before-save
  "Adjusts account data for saving in the database"
  [storage account]
  (-> account
      (update-in [:balance] (fnil identity 0M))
      (update-in [:type] name)
      (update-in [:tags] #(if (seq %)
                            (mapv name %)
                            nil))
      (dissoc :commodity)))

(defn- after-read
  "Adjusts account data read from the database for use"
  ([account] (after-read nil account))
  ([_ account]
   (-> account
       (update-in [:type] keyword)
       (update-in [:tags] #(->> %
                                (map keyword)
                                set))
       (assoc :commodity {:name (:commodity-name account)
                          :symbol (:commodity-symbol account)
                          :type (keyword (:commodity-type account))
                          :default (= (:commodity-id account) (-> account
                                                                  :entity-settings
                                                                  safe-read-string
                                                                  :default-commodity-id))
                          #_:exchange #_(:commodity-exchange account)})
       (dissoc :commodity-name
               :commodity-symbol
               :commodity-type
               :commodity-exchange
               :entity-settings)
       (authorization/tag-resource :account)
       (cond->
         (and ; Remove :parent-id if it's nil
           (contains? account :parent-id)
           (nil? (:parent-id account)))
         (dissoc :parent-id)))))

(defn- name-is-unique?
  [storage {:keys [id parent-id name entity-id]}]
  (->> (select-accounts storage {:entity-id entity-id
                                 :name name})
       (remove #(= (:id %) id))
       (filter #(= (:parent-id %) parent-id))
       empty?))

(defn- parent-has-same-type?
  "Validation rule that ensure an account
  has the same type as its parent"
  [storage {:keys [parent-id type] :as model}]
  (or (nil? parent-id)
      (= type
         (:type (find-by-id storage parent-id)))))

(defn- validation-rules
  [storage]
  (map (fn [{:keys [path message val-fn]}]
         (validation/create-rule (partial val-fn storage)
                                 path
                                 message))
       [{:val-fn name-is-unique?
         :path [:name]
         :message "Name is already in use"}
        {:val-fn parent-has-same-type?
         :path [:type]
         :message "Type must match the parent type"}]))

(def create
  (create-fn {:before-save before-save
              :after-read after-read
              :create create-account
              :before-validation before-validation
              :rules-fn validation-rules
              :coercion-rules coercion-rules
              :spec ::new-account}))

(defn find-by-id
  "Returns the account having the specified id"
  [storage-spec id]
  (when id
    (with-storage [s storage-spec]
      (after-read (find-account-by-id s id)))))

(defn find-by-name
  "Returns the account having the specified name"
  [storage-spec entity-id account-name]
  (with-storage [s storage-spec]
    (after-read
      (find-account-by-entity-id-and-name s
                                          entity-id
                                          account-name))))

(defn reload
  "Returns a fresh copy of the specified account from the data store"
  [storage-spec {:keys [id]}]
  (find-by-id storage-spec id))

(defn select-by-entity-id
  "Returns a list of all accounts in the system"
  ([storage-spec entity-id] (select-by-entity-id storage-spec entity-id {}))
  ([storage-spec entity-id options]
   (with-storage [s storage-spec]
     (let [types (or (:types options)
                     (set account-types))]
       (->> (select-accounts s {:entity-id entity-id})
            (map after-read)
            (filter #(types (:type %))))))))

(defn- append-path
  [account parent]
  (assoc account :path (str (:path parent) "/" (:name account))))

(defn- append-children
  [account all-accounts]
  (let [children (->> all-accounts
                      (filter #(= (:id account) (:parent-id %)))
                      (map #(append-path % account))
                      (map #(append-children % all-accounts))
                      (sort-by :name)
                      vec)]
    (assoc account :children children
                   :children-balance (reduce #(+ %1 (:balance %2) (:children-balance %2))
                                             0
                                             children))))

(defn select-nested-by-entity-id
  "Returns the accounts for the entity with children nested under
  parents and parents grouped by type"
  ([storage-spec entity-id]
   (select-nested-by-entity-id storage-spec entity-id account-types))
  ([storage-spec entity-id types]
   (let [all (select-by-entity-id storage-spec entity-id)
         grouped (->> all
                      (remove :parent-id)
                      (map #(assoc % :path (:name %)))
                      (map #(append-children % all))
                      (group-by :type))]
     (mapv #(hash-map :type % :accounts (or
                                          (->> grouped
                                               %
                                               (sort-by :name)
                                               vec)
                                          []))
           types))))

(def update
  (update-fn {:before-save before-save
              :after-read after-read
              :update update-account
              :find find-by-id
              :spec ::existing-account
              :coercion-rules coercion-rules
              :rules-fn validation-rules}))

(defn delete
  "Removes the account from the system"
  [storage-spec id]
  (with-storage [s storage-spec]
    (delete-account s id)))

(defn left-side?
  "Returns truthy if the specified account is asset or expense, falsey if anything else"
  [account]
  (#{:asset :expense} (:type account)))

(defn polarize-amount
  "Adjusts the polarity of an amount as appropriate given
  a transaction item action and the type of the associated account"
  [transaction-item account]
  (let [polarizer (* (if (left-side? account) 1 -1)
                     (if (= :debit (:action transaction-item)) 1 -1))]
    (* (:amount transaction-item) polarizer)))

(defn search
  [storage-spec criteria]
  (with-storage [s storage-spec]
    (->> criteria
         (select-accounts s)
         (map after-read))))

(authorization/allow :account [:new :create :show :edit :update :delete]
       (fn [user resource]
         (user-owns-entity? user (:entity-id resource))))
