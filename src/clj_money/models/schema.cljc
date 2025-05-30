(ns clj-money.models.schema
  (:require [clojure.spec.alpha :as s]
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])))

(s/def ::id keyword?)
(s/def ::type keyword?)
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
(s/def ::join-spec (s/keys :req-un [::id
                                    ::columns]))
(s/def ::ref (s/or :simple keyword?
                   :complex ::join-spec))
(s/def ::refs (s/coll-of ::ref
                         :min-count 1
                         :kind set?))
(s/def ::model (s/keys :req-un [::id
                                ::fields]
                       :opt-un [::refs]))
(def models
        [{:id :user
          :fields #{{:id :email
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
                     :type :date-time}}}
         {:id :identity
          :fields #{{:id :provider
                     :type :string}
                    {:id :provider-id
                     :type :string}}
          :refs #{:user}}
         {:id :import
          :fields #{{:id :entity-name
                     :type :string}
                    {:id :image-ids
                     :type :string}
                    {:id :options
                     :type :map}
                    {:id :progress
                     :type :map
                     :transient? true}}
          :refs #{:user
                  :image}}
         {:id :image
          :fields #{{:id :original-filename
                     :type :string}
                    {:id :uuid
                     :type :string}
                    {:id :content-type
                     :type :string}}
          :refs #{:user}}
         {:id :entity
          :fields #{{:id :name
                     :type :string}
                    {:id :price-date-range
                     :type :tuple
                     :transient? true}
                    {:id :transaction-date-range
                     :type :tuple
                     :transient? true}}
          :refs #{:user}}
         {:id :grant
          :fields #{{:id :permissions
                     :type :map}}
          :refs #{:user :entity}}
         {:id :commodity
          :fields #{{:id :type
                     :type :keyword}
                    {:id :name
                     :type :string}
                    {:id :symbol
                     :type :string}
                    {:id :exchange
                     :type :keyword}
                    {:id :price-config
                     :type :map}
                    {:id :price-date-range
                     :type :tuple
                     :transient? true}}
          :refs #{:entity}}
         {:id :price
          :fields #{{:id :trade-date
                     :type :date}
                    {:id :value
                     :type :decimal}}
          :refs #{:commodity}}
         {:id :account
          :fields #{{:id :name
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
                  :parent}}
         {:id :transaction
          :fields #{{:id :transaction-date
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
                     :transient? true}}
          :refs #{:entity
                  :scheduled-transaction}}
         {:id :transaction-item
          :fields #{{:id :action
                     :type :keyword}
                    {:id :quantity
                     :type :decimal}
                    {:id :memo
                     :type :string}
                    {:id :index
                     :type :string
                     :transient? true}
                    {:id :quantity
                     :type :string
                     :transient? true}
                    {:id :balance
                     :type :string
                     :transient? true}
                    {:id :value
                     :type :string
                     :transient? true}
                    {:id :negative
                     :type :string
                     :transient? true}}
          :refs #{:account
                  :reconciliation
                  {:id :transaction
                   :columns #{:transaction-date
                              [:id :transaction-id]}}}}
         {:id :lot
          :fields #{{:id :shares-purchased
                     :type :decimal}
                    {:id :purchase-date
                     :type :date}
                    {:id :purchase-price
                     :type :decimal}
                    {:id :shared-owned
                     :type :string
                     :transient? true}}
          :refs #{:account
                  :commodity}}
         {:id :lot-item
          :fields #{{:id :action
                     :type :keyword}
                    {:id :shares
                     :type :decimal}
                    {:id :price
                     :type :decimal}}
          :refs #{:lot
                  {:id :transaction
                   :columns #{:transaction-date
                              [:id :transaction-id]}}}}
         {:id :budget
          :fields #{{:id :name
                     :type :string}
                    {:id :start-date
                     :type :date}
                    {:id :period
                     :type :tuple}
                    {:id :end-date
                     :type :date
                     :transient? true}}
          :refs #{:entity}}
         {:id :budget-item
          :fields #{{:id :periods
                     :type :vector}
                    {:id :spec
                     :type :map}}
          :refs #{:budget
                  :account}}
         {:id :scheduled-transaction
          :fields #{{:id :description
                     :type :string}
                    {:id :start-date
                     :type :date}
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
                     :transient? true}}
          :refs #{:entity}}
         {:id :scheduled-transaction-item
          :fields #{{:id :action
                     :type :keyword}
                    {:id :quantity
                     :type :decimal}
                    {:id :memo
                     :type :string}}
          :refs #{:scheduled-transaction
                  :account}}
         {:id :attachment
          :fields #{{:id :caption
                     :type :string}}
          :refs #{:image
                  {:id :transaction
                   :columns #{:transaction-date
                              [:id :transaction-id]}}}}
         {:id :reconciliation
          :fields #{{:id :status
                     :type :keyword}
                    {:id :balance
                     :type :decimal}
                    {:id :end-of-period
                     :type :date}}
          :refs #{:account}}
         {:id :cached-price
          :fields #{{:id :trade-date
                     :type :date}
                    {:id :symbol
                     :type :keyword}
                    {:id :exchange
                     :type :keyword}
                    {:id :value
                     :type :decimal}}}])

(assert (s/valid? (s/coll-of ::model) models)
        "The schema is not valid")

(def ref-id (some-fn :id identity))
