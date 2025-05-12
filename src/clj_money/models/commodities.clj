(ns clj-money.models.commodities
  (:refer-clojure :exclude [update count find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [dgknght.app-lib.core :refer [assoc-if]]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util]
            [clj-money.dates :as dates]
            [clj-money.models :as models]
            [clj-money.models.propagation :as prop]))

(defn- name-is-unique?
  [{:keys [id] :as commodity}]
  (if (every? #(% commodity)
              [:commodity/name
               :commodity/exchange
               :commodity/entity])
    (= 0 (models/count
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
    (models/count
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

(s/def :commodity/entity ::models/model-ref)
(s/def :commodity/name string?)
(s/def :commodity/symbol string?)
(s/def :commodity/type #{:currency :stock :fund})
(s/def :commodity/price-date-range (s/tuple dates/local-date? dates/local-date?))
(s/def :commodity/exchange (s/nilable #{:amex :nasdaq :nyse :otc}))

(s/def :price-config/enabled boolean?)
(s/def :commodity/price-config (s/keys :req [:price-config/enabled]))

(s/def ::models/commodity (s/and (s/keys :req [:commodity/type
                                               :commodity/name
                                               :commodity/symbol
                                               :commodity/entity]
                                         :opt [:commodity/price-config
                                               :commodity/exchange
                                               :commodity/price-date-range])
                                 name-is-unique?
                                 symbol-is-unique?
                                 exchange-is-satisfied?))

(defmethod models/before-validation :commodity
  [comm]
  (update-in comm [:commodity/price-config] #(or % {:price-config/enabled false})))

(defn propagate-all
  ([opts]
   (doseq [entity (models/select (util/model-type {} :entity))]
     (propagate-all entity opts)))
  ([entity _opts]
   {:pre [entity]}
   (when-not (get-in entity [:entity/settings
                             :settings/default-commodity])
     (when-let [currencies (seq
                             (models/select {:commodity/entity entity
                                             :commodity/type :currency}))]
       (when (< 1 (clojure.core/count currencies))
         (log/warnf "Found multiple currencies for entity %s, defaulting to %s."
                    (select-keys entity [:id :entity/name])
                    (select-keys (first currencies) [:id :commodity/name :commodity/symbol])))
       (models/put-many [(assoc-in entity
                                   [:entity/settings
                                    :settings/default-commodity]
                                   (util/->model-ref (first currencies)))])))))

(prop/add-full-propagation propagate-all :priority 5)
