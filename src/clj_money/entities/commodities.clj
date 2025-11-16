(ns clj-money.entities.commodities
  (:refer-clojure :exclude [update count find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
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

(s/def :price-config/enabled boolean?)
(s/def :commodity/price-config (s/keys :req [:price-config/enabled]))

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(s/def ::entities/commodity
  (s/and (s/keys :req [:commodity/type
                       :commodity/name
                       :commodity/symbol
                       :commodity/entity]
                 :opt [:commodity/price-config
                       :commodity/exchange
                       :commodity/price-date-range])
         name-is-unique?
         symbol-is-unique?
         exchange-is-satisfied?))

(defmethod entities/before-validation :commodity
  [comm]
  (update-in comm [:commodity/price-config] #(or % {:price-config/enabled false})))

(defn propagate-all
  [entity _opts]
  {:pre [entity]}
  (log/debugf "[propagation] start entity %s"
              (:entity/name entity))
  (if (get-in entity [:entity/settings
                      :settings/default-commodity])
    (do (log/info "entity already has a default commodity")
        entity)
    (if-let [currencies (seq
                          (entities/select {:commodity/entity entity
                                          :commodity/type :currency}))]
      (do (when (< 1 (clojure.core/count currencies))
            (log/warnf "Found multiple currencies for entity %s, defaulting to %s."
                       (select-keys entity [:id :entity/name])
                       (select-keys (first currencies) [:id :commodity/name :commodity/symbol])))
          (let [updated (entities/put (assoc-in entity
                                              [:entity/settings
                                               :settings/default-commodity]
                                              (util/->entity-ref (first currencies))))]
            (log/infof "[propagation] finish entity %s"
                       (:entity/name entity))
            updated))
      (do (log/info "No currencies to set as the default commodity")
          entity))))

(prop/add-full-propagation propagate-all :priority 5)
