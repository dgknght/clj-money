(ns clj-money.models.schema
  (:require [clojure.spec.alpha :as s]))

(s/def ::type keyword?)
(s/def ::fields (s/coll-of keyword?
                           :min-count 1
                           :kind set?))
(s/def ::refs (s/coll-of keyword?
                            :min-count 1
                            :kind set?))
(s/def ::transient-fields (s/coll-of keyword?
                                     :min-count 1
                                     :kind set?))
(s/def ::model (s/keys :req-un [::type
                                ::fields]
                       :opt-un [::refs
                                ::transient-fields]))
(def schema
  [{:type :user
    :fields #{:email
              :password
              :first-name
              :last-name
              :password-reset-token
              :token-expires-at}}
   {:type :identity
    :fields #{:provider
              :provider-id}
    :refs #{:user}}
   {:type :import
    :fields #{:entity-name
              :image-ids
              :options}
    :transient-fields #{:progress}
    :refs #{:user
            :image}}
   {:type :image
    :fields #{:original-filename
              :uuid
              :content-type}
    :refs #{:user}}
   {:type :entity
    :fields #{:name}
    :transient-fields #{:price-date-range
                        :transaction-date-range}
    :refs #{:user}}
   {:type :grant
    :fields #{:permissions}
    :refs #{:user :entity}}
   {:type :commodity
    :fields #{:type
              :name
              :symbol
              :exchange
              :price-config}
    :transient-fields #{:price-date-range}
    :refs #{:entity}}
   {:type :price
    :fields #{:trade-date
              :price}
    :refs #{:commodity}}
   {:type :account
    :fields #{:name
              :type
              :allocations
              :user-tags
              :system-tags
              :hidden}
    :transient-fields #{:quantity
                        :value
                        :price-as-of
                        :transaction-date-range}
    :refs #{:entity
            :commodity
            :parent}}
   {:type :transaction
    :fields #{:transaction-date
              :description
              :memo}
    :transient-fields #{:value
                        :attachment-count}
    :refs #{:entity
            :scheduled-transaction}}
   {:type :transaction-item
    :fields #{:action
              :quantity
              :memo}
    :transient-fields #{:index
                        :quantity
                        :balance
                        :value
                        :negative}
    :refs #{:account
            :transaction
            :reconciliation}}
   {:type :lot
    :fields #{:shares-purchased
              :purchase-date
              :purchase-price}
    :transient-fields #{:shared-owned}
    :refs #{:account
            :commodity}}
   {:type :lot-item
    :fields #{:action
              :shares
              :price}
    :refs #{:lot
            :transaction}}
   {:type :budget
    :fields #{:name
              :start-date
              :period}
    :transient-fields #{:end-date}
    :refs #{:entity}}
   {:type :budget-item
    :fields #{:periods
              :spec}
    :refs #{:budget
            :account}}
   {:type :scheduled-transaction
    :fields #{:description
              :start-date
              :end-date
              :enabled
              :period
              :memo}
    :transient-fields #{:last-occurrence}
    :refs #{}}
   {:type :scheduled-transaction-item
    :fields #{:action
              :quantity
              :memo}
    :refs #{:scheduled-transaction
            :account}}
   {:type :attachment
    :fields #{:caption}
    :refs #{:transaction
            :image}}])
