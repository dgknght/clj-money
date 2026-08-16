(ns clj-money.entities.entities
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [assoc-if
                                          present?]]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util :refer [id=]]
            [clj-money.dates :as dates]
            [clj-money.entities :as entities]))

(defn- name-is-unique?
  [{:as entity :keys [id]}]
  (zero? (entities/count
           (-> entity
               (select-keys [:entity/name :entity/user])
               (assoc-if :id (when id [:!= id]))))))
(v/reg-spec name-is-unique? {:message "%s is already in use"
                             :path [:entity/name]})

(defn- default-commodity-belongs-to-entity?
  [{:as entity {:settings/keys [default-commodity]} :entity/settings}]
  (or (nil? default-commodity)
      (id= entity
           (:commodity/entity (entities/resolve-ref default-commodity)))))
(v/reg-spec default-commodity-belongs-to-entity?
            {:message "Default commodity must belong to the entity"
             :path [:entity/settings :settings/default-commodity]})

(s/def :entity/name (s/and string?
                     present?))
(s/def :entity/user ::entities/entity-ref)
(s/def :entity/price-date-range (s/nilable (s/tuple dates/local-date? dates/local-date?)))
(s/def :entity/transaction-date-range (s/nilable (s/tuple dates/local-date? dates/local-date?)))
(s/def :settings/monitored-accounts (s/coll-of ::entities/entity-ref :kind set?))
(s/def :settings/inventory-method #{:fifo :lifo})
(s/def :settings/default-commodity ::entities/entity-ref)
(s/def :settings/lt-capital-gains-account ::entities/entity-ref)
(s/def :settings/st-capital-gains-account ::entities/entity-ref)
(s/def :settings/lt-capital-loss-account ::entities/entity-ref)
(s/def :settings/st-capital-loss-account ::entities/entity-ref)
(s/def :settings/budget-tags (s/coll-of keyword? :kind vector?))
(s/def :entity/settings (s/nilable
                          (s/keys :opt [:settings/inventory-method
                                        :settings/monitored-accounts
                                        :settings/default-commodity
                                        :settings/budget-tags])))

(s/def ::entities/entity (s/and (s/keys :req [:entity/name
                                              :entity/user]
                                        :opt [:entity/settings
                                              :entity/price-date-range
                                              :entity/transaction-date-range])
                                name-is-unique?
                                default-commodity-belongs-to-entity?))
