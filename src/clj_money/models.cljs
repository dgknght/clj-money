(ns clj-money.models
  (:require [cljs.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.util :as util]))

(def models
  {:commodity {:keys #{:commodity/name
                       :commodity/symbol
                       :commodity/exchange
                       :commodity/type
                       :commodity/price-config}
               :references #{:commodity/entity}}
   :price {:keys #{:price/price
                   :price/trade-date}
           :references #{:price/commodity}}
   :account {:keys #{:id
                     :account/name
                     :account/type
                     :account/allocations
                     :account/trading
                     :account/system-tags
                     :account/user-tags}
             :references #{:account/commodity
                           :account/entity
                           :account/parent}}
   :transaction {:keys #{:transaction/transaction-date
                         :transaction/description
                         :transaction/memo
                         :transaction/value
                         :transaction/attachment-count}
                 :references #{:transaction/entity
                               :transaction/scheduled-transaction}
                 :collections {:transaction/items :transaction-item}}
   :transaction-item {:keys #{:transaction-item/quantity
                              :transaction-item/action
                              :transaction-item/value
                              :transaction-item/balance
                              :transaction-item/index
                              :transaction-item/negative}
                      :references #{:transaction-item/account
                                    :transaction-item/transaction
                                    :transaction-item/reconciliation}}
   :trade {:keys #{:trade/date
                   :trade/action
                   :trade/shares
                   :trade/value}
           :references #{:trade/account
                         :trade/commodity}}})

(declare prune)

(defn- simplify-refs
  [m model-type]
  (reduce (fn [model k]
            (update-in-if model [k] util/->model-ref))
          m
          (-> models model-type :references)))

(defn- process-collections
  [m model-type]
  (reduce (fn [model [k v]]
            (update-in model
                       [k]
                       (fn [coll]
                         (map #(prune % v)
                              coll))))
          m
          (-> models model-type :collections)))

(defn- apply-transformations
  [m model-type]
  (-> m
      (simplify-refs model-type)
      (process-collections model-type)))

(defn- include
  [model-type]
  (cons :id
        (concat (-> models model-type :keys)
                (-> models model-type :references)
                (-> models model-type :collections keys))))

(defn prune
  [m model-type & {:keys [exclude]}]
  (assert (models model-type) (str "Unrecognized model type: " model-type))
  (-> m
      (select-keys (include model-type))
      (dissoc exclude)
      (apply-transformations model-type)))
