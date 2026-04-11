(ns clj-money.entities.commodities
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [assoc-if]]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util]
            [clj-money.dates :as dates]
            [clj-money.entities :as entities]
            [clj-money.entities.propagation :as prop]))

(defn- name-is-unique?
  [{:keys [id] :as commodity}]
  (if (every? #(% commodity)
              [:commodity/name
               :commodity/exchange
               :commodity/entity])
    (= 0 (entities/count
           (-> commodity
               (select-keys [:commodity/name
                             :commodity/type
                             :commodity/exchange
                             :commodity/entity])
               (assoc-if :id (when id [:!= id])))))
    true))

(v/reg-spec name-is-unique? {:message "%s is already in use"
                             :path [:commodity/name]})

(defn- symbol-is-unique?
  [{:keys [id] :as commodity}]
  (zero?
    (entities/count
      (-> commodity
          (select-keys [:commodity/symbol
                        :commodity/type
                        :commodity/exchange
                        :commodity/entity])
          (assoc-if :id (when id [:!= id]))))))

(v/reg-spec symbol-is-unique? {:message "%s is already in use"
                               :path [:commodity/symbol]})

(defn- exchange-is-satisfied?
  [{:commodity/keys [type exchange]}]
  (or (#{:fund :currency} type)
      exchange))
(v/reg-spec exchange-is-satisfied? {:message "%s is required"
                                    :path [:commodity/exchange]})

(s/def :commodity/entity ::entities/entity-ref)
(s/def :commodity/name string?)
(s/def :commodity/symbol string?)
(s/def :commodity/type #{:currency :stock :fund})
(s/def :commodity/price-date-range (s/nilable (s/tuple dates/local-date? dates/local-date?)))
(s/def :commodity/exchange (s/nilable #{:amex :nasdaq :nyse :otc}))
(s/def :commodity/shares-owned decimal?)

(s/def :price-config/enabled boolean?)
(s/def :commodity/price-config (s/keys :req [:price-config/enabled]))

(s/def ::entities/commodity
  (s/and (s/keys :req [:commodity/type
                       :commodity/name
                       :commodity/symbol
                       :commodity/entity]
                 :opt [:commodity/price-config
                       :commodity/exchange
                       :commodity/price-date-range
                       :commodity/shares-owned])
         name-is-unique?
         symbol-is-unique?
         exchange-is-satisfied?))

(defmethod entities/before-validation :commodity
  [comm]
  (update-in comm [:commodity/price-config] #(or % {:price-config/enabled false})))

(defn with-shares-owned
  "Returns the commodity with :commodity/shares-owned set to the
  sum of shares-owned across all lots for that commodity."
  [commodity]
  (let [shares (->> (entities/select
                      #:lot{:commodity (util/->entity-ref commodity)
                            :shares-owned [:> 0]})
                    (map :lot/shares-owned)
                    (reduce + 0M))]
    (assoc commodity :commodity/shares-owned shares)))

(defn recalc-shares-owned
  "Recalculates and saves the shares-owned total for a commodity."
  [commodity]
  (entities/put (with-shares-owned commodity)))

(defn- assign-default-commodity
  [entity]
  (if-let [currency (entities/find-by {:commodity/entity entity
                                       :commodity/type :currency})]
    (entities/put (assoc-in entity
                            [:entity/settings
                             :settings/default-commodity]
                            (util/->entity-ref currency)))
    entity))

(defn propagate-all
  [entity _opts]
  {:pre [entity]}
  (doseq [commodity (entities/select {:commodity/entity entity})]
    (recalc-shares-owned commodity))
  (if (get-in entity [:entity/settings
                      :settings/default-commodity])
    entity
    (assign-default-commodity entity)))

(prop/add-full-propagation propagate-all :priority 5)
