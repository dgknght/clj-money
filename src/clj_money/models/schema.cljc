(ns clj-money.models.schema
  (:require [clojure.spec.alpha :as s]))

(s/def ::id keyword?)
(s/def ::fields (s/coll-of keyword?
                           :min-count 1
                           :kind set?))
(s/def ::refs (s/coll-of keyword?
                            :min-count 1
                            :kind set?))
(s/def ::transient-fields (s/coll-of keyword?
                                     :min-count 1
                                     :kind set?))
(s/def ::model (s/keys :req-un [::id
                                ::fields]
                       :opt-un [::refs
                                ::transient-fields]))
(def models
  [{:id :user
    :fields #{:email
              :password
              :first-name
              :last-name
              :password-reset-token
              :token-expires-at}}
   {:id :identity
    :fields #{:provider
              :provider-id}
    :refs #{:user}}
   {:id :import
    :fields #{:entity-name
              :image-ids
              :options}
    :transient-fields #{:progress}
    :refs #{:user
            :image}}
   {:id :image
    :fields #{:original-filename
              :uuid
              :content-type}
    :refs #{:user}}
   {:id :entity
    :fields #{:name}
    :transient-fields #{:price-date-range
                        :transaction-date-range}
    :refs #{:user}}
   {:id :grant
    :fields #{:permissions}
    :refs #{:user :entity}}
   {:id :commodity
    :fields #{:type
              :name
              :symbol
              :exchange
              :price-config}
    :transient-fields #{:price-date-range}
    :refs #{:entity}}
   {:id :price
    :fields #{:trade-date
              :price}
    :refs #{:commodity}}
   {:id :account
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
   {:id :transaction
    :fields #{:transaction-date
              :description
              :memo}
    :transient-fields #{:value
                        :attachment-count}
    :refs #{:entity
            :scheduled-transaction}}
   {:id :transaction-item
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
   {:id :lot
    :fields #{:shares-purchased
              :purchase-date
              :purchase-price}
    :transient-fields #{:shared-owned}
    :refs #{:account
            :commodity}}
   {:id :lot-item
    :fields #{:action
              :shares
              :price}
    :refs #{:lot
            :transaction}}
   {:id :budget
    :fields #{:name
              :start-date
              :period}
    :transient-fields #{:end-date}
    :refs #{:entity}}
   {:id :budget-item
    :fields #{:periods
              :spec}
    :refs #{:budget
            :account}}
   {:id :scheduled-transaction
    :fields #{:description
              :start-date
              :end-date
              :enabled
              :period
              :memo}
    :transient-fields #{:last-occurrence}
    :refs #{:entity}}
   {:id :scheduled-transaction-item
    :fields #{:action
              :quantity
              :memo}
    :refs #{:scheduled-transaction
            :account}}
   {:id :attachment
    :fields #{:caption}
    :refs #{:transaction
            :image}}])

(assert (s/valid? (s/coll-of ::model) models))
