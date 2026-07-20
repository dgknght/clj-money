(ns clj-money.entities.schema
  (:require [clojure.spec.alpha :as s]
            [dgknght.app-lib.core :refer [update-in-if]]
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])))

(s/def ::id keyword?)
(s/def ::type (s/or :singular keyword?
                    :plural (s/tuple keyword?)))
(s/def ::transient? boolean?)
(s/def ::field (s/keys :req-un [::id
                                ::type]
                       :opt-un [::transient?]))
(s/def ::fields (s/coll-of ::field
                           :min-count 1
                           :kind set?))
(s/def ::column-spec (s/or :simple keyword?
                           :complex (s/tuple keyword? keyword?)))
(s/def ::columns (s/coll-of ::column-spec))
(s/def ::join-spec (s/keys :req-un [::id]
                           :opt-un [::columns
                                    ::type]))
(s/def ::ref (s/or :simple keyword?
                   :complex ::join-spec))
(s/def ::refs (s/coll-of ::ref
                         :min-count 1
                         :kind set?))
(s/def ::primary-key (s/coll-of keyword?))
(s/def ::entity (s/keys :req-un [::fields]
                        :opt-un [::refs
                                 ::primary-key]))
(s/def ::entities (s/map-of keyword? ::entity))

(def entities
  {:user {:fields #{{:id :email
                     :type :string}
                    {:id :password
                     :type :string}
                    {:id :first-name
                     :type :string}
                    {:id :last-name
                     :type :string}
                    {:id :password-reset-token
                     :type :string}
                    {:id :token-expires-at
                     :type :date-time}
                    {:id :roles
                     :type :set}}}
   :identity {:fields #{{:id :provider
                         :type :keyword}
                        {:id :provider-id
                         :type :string}}
              :refs #{:user}}
   :import {:fields #{{:id :entity-name
                       :type :string}
                      {:id :image-ids
                       :type :string}
                      {:id :options
                       :type :map}
                      {:id :progress
                       :type :map
                       :transient? true}}
            :refs #{:user
                    {:id :images
                     :type [:image]}}} ; the vector indicates a vector of images
   :image {:fields #{{:id :original-filename
                      :type :string}
                     {:id :uuid
                      :type :string}
                     {:id :content-type
                      :type :string}}
           :refs #{:user}}
   :entity {:fields #{{:id :name
                       :type :string}
                      {:id :price-date-range
                       :type :tuple
                       :transient? true}
                      {:id :transaction-date-range
                       :type :tuple
                       :transient? true}
                      {:id :settings
                       :type :map}}
            :refs #{:user}}
   :grant {:fields #{{:id :permissions
                      :type :map}}
           :refs #{:user :entity}}
   :commodity {:fields #{{:id :type
                          :type :keyword}
                         {:id :name
                          :type :string}
                         {:id :symbol
                          :type :string}
                         {:id :exchange
                          :type :keyword}
                         {:id :price-config
                          :type :map}
                         {:id :shares-owned
                          :type :decimal}
                         {:id :price-date-range
                          :type :tuple
                          :transient? true}}
               :refs #{:entity}}
   :price {:primary-key [:trade-date :id]
           :fields #{{:id :trade-date
                      :type :date}
                     {:id :value
                      :type :decimal}}
           :refs #{:commodity}}
   :account {:fields #{{:id :name
                        :type :string}
                       {:id :type
                        :type :keyword}
                       {:id :allocations
                        :type :map}
                       {:id :user-tags
                        :type :set}
                       {:id :system-tags
                        :type :set}
                       {:id :hidden
                        :type :boolean}
                       {:id :quantity
                        :type :decimal
                        :transient? true}
                       {:id :value
                        :type :decimal
                        :transient? true}
                       {:id :price-as-of
                        :type :decimal
                        :transient? true}
                       {:id :transaction-date-range
                        :type :tuple
                        :transient? true}}
             :refs #{:entity
                     :commodity
                     {:id :parent
                      :type :account}}}
   :transaction {:fields #{{:id :transaction-date
                            :type :date}
                           {:id :description
                            :type :string}
                           {:id :memo
                            :type :string}
                           {:id :value
                            :type :decimal
                            :transient? true}
                           {:id :attachment-count
                            :type :string
                            :transient? true}
                           {:id :items
                            :type :vector}}
                 :refs #{:entity
                         :scheduled-transaction
                         {:id :items
                          :type [:transaction-item]
                          :component true}
                         {:id :lot-items
                          :type [:lot-item]
                          :component true}}}
   :transaction-item {:fields #{{:id :quantity
                                 :type :decimal}
                                {:id :value
                                 :type :decimal}
                                {:id :balance
                                 :type :decimal}
                                {:id :index
                                 :type :integer}
                                {:id :action
                                 :type :keyword}
                                {:id :memo
                                 :type :string}}
                      :refs #{:account
                              :reconciliation}}
   :lot {:fields #{{:id :shares-purchased
                    :type :decimal}
                   {:id :purchase-date
                    :type :date}
                   {:id :purchase-price
                    :type :decimal}
                   {:id :shares-owned
                    :type :string
                    :transient? true}}
         :refs #{:account
                 :commodity}}
   :lot-note {:fields #{{:id :transaction-date
                         :type :date}
                        {:id :memo
                         :type :string}}
              :refs #{{:id :lots :type [:lot]}}}
   :lot-item {:fields #{{:id :action
                         :type :keyword}
                        {:id :shares
                         :type :decimal}
                        {:id :price
                         :type :decimal}}
              :refs #{:lot}}
   :budget {:fields #{{:id :name
                       :type :string}
                      {:id :start-date
                       :type :date}
                      {:id :period
                       :type :tuple}
                      {:id :end-date
                       :type :date
                       :transient? true}
                      {:id :auto-create-start-date
                       :type :date
                       :transient? true}}
            :refs #{:entity
                    {:id :items
                     :type [:budget-item]
                     :component true}}}
   :budget-item {:fields #{{:id :periods
                            :type :vector}
                           {:id :spec
                            :type :map}}
                 :refs #{:account}}
   :scheduled-transaction {:fields #{{:id :description
                                      :type :string}
                                     {:id :start-date
                                      :type :date}
                                     {:id :date-spec
                                      :type :map}
                                     {:id :end-date
                                      :type :date}
                                     {:id :enabled
                                      :type :boolean}
                                     {:id :period
                                      :type :tuple}
                                     {:id :memo
                                      :type :string}
                                     {:id :last-occurrence
                                      :type :string
                                      :transient? true}
                                     {:id :items
                                      :type :vector}}
                           :refs #{:entity
                                   {:id :items
                                    :type [:scheduled-transaction-item]
                                    :component true}}}
   :scheduled-transaction-item {:fields #{{:id :action
                                           :type :keyword}
                                          {:id :quantity
                                           :type :decimal}
                                          {:id :memo
                                           :type :string}}
                                :refs #{:account}}
   :attachment {:fields #{{:id :caption
                           :type :string}}
                :refs #{:image
                        :transaction}}
   :reconciliation {:fields #{{:id :status
                               :type :keyword}
                              {:id :balance
                               :type :decimal}
                              {:id :end-of-period
                               :type :date}
                              {:id :items
                               :type :vector}}
                    :refs #{:account}}
   :cached-price {:primary-key [:trade-date :id]
                  :fields #{{:id :trade-date
                             :type :date}
                            {:id :symbol
                             :type :string}
                            {:id :exchange
                             :type :keyword}
                            {:id :value
                             :type :decimal}}}
   :invitation {:fields #{{:id :recipient
                           :type :string}
                          {:id :note
                           :type :string}
                          {:id :status
                           :type :keyword}
                          {:id :token
                           :type :string}
                          {:id :expires-at
                           :type :date-time}}
                :refs #{{:id :invited-by :type :user}
                        :user}}})

(assert (s/valid? ::entities entities)
        "The schema is not valid")

(def ^:private overrides
  {:sql {:transaction {:refs #{:entity
                               :scheduled-transaction}}
         :transaction-item {:refs #{:transaction
                                    :account
                                    :reconciliation}}

         :lot-item {:refs #{:lot
                            :transaction}}

         :scheduled-transaction {:refs #{:entity}}
         :scheduled-transaction-item {:refs #{:scheduled-transaction
                                              :account}}

         :budget {:refs #{:entity}}
         :budget-item {:refs #{:budget
                               :account}}}})

(defn- resolve-merge-conflict
  [v1 v2]
  (if (set? v1)
    v2
    (merge-with resolve-merge-conflict v1 v2)))

(defn build
  [override]
  (merge-with resolve-merge-conflict
              entities
              (overrides override)))

(def ref-id (some-fn :id identity))

(defn relationship-ref-type
  "Returns the entity type for a relationship."
  [ref]
  (if (and (map? ref) (vector? (:type ref)))
    (first (:type ref))
    (ref-id ref)))

(defn relationship-tuple
  "Returns a function that takes an entity reference and
  returns a type with the referenced type in the 1st position,
  the referencing type in the 2nd position, and an optional
  attribute override in the 3rd position (if the type of the
  referenced entity is not also the attribute)"
  [id]
  (fn [ref]
    (if (keyword? ref)
      [ref id]
      (if-let [t (:type ref)]
        [(if (vector? t) (first t) t) id (:id ref)]
        [(:id ref) id]))))

(defn- relationship-tuples
  [[id {:keys [refs]}]]
  (map (relationship-tuple id) refs))

(def relationships
  (->> entities
       (mapcat relationship-tuples)
       set))

(defn- extract-attributes
  [[entity-id {:keys [fields refs]}]]
  (concat (map (fn [{:keys [id]}]
                 (keyword (name entity-id)
                          (name id)))
               fields)
          (map (fn [ref]
                 (keyword (name entity-id)
                          (name (or (:id ref) ref))))
               refs)))

(def attributes
  (->> entities
       (map (juxt first extract-attributes))
       (into {})))

(defn- extract-reference-attributes
  [[entity-id {:keys [refs]}]]
  (->> refs
       (remove :component)
       (map (fn [ref]
              (keyword (name entity-id)
                       (name (or (:id ref) ref)))))))

(def reference-attributes
  (->> entities
       (map (juxt first extract-reference-attributes))
       (into {})))

(defn- simplify-references
  [entity entity-type]
  (reduce (fn [m ref]
            (update-in-if m [ref] #(select-keys % [:id])))
          entity
          (reference-attributes entity-type)))

(defn prune
  "Given a entity, remove keys that don't belong to the entity
  and reduce references to a simple entity ref"
  [entity entity-type & {:keys [allow]}]
  (-> entity
      (select-keys (cons :id (concat (attributes entity-type)
                                     allow)))
      (simplify-references entity-type)))
