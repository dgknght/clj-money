(ns clj-money.models
  (:require [dgknght.app-lib.core :refer [update-in-if]]
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
                           :account/parent}}})

(defn- apply-transformations
  [m model-type]
  (reduce (fn [model k]
            (update-in-if model [k] util/->model-ref))
          m
          (-> models model-type :references)))

(defn- include
  [model-type]
  (cons :id
        (concat (-> models model-type :keys)
                (-> models model-type :references))))

(defn prune
  [m model-type & {:keys [exclude]}]
  (assert (models model-type) (str "Unrecognized model type: " model-type))
  (-> m
      (select-keys (include model-type))
      (dissoc exclude)
      (apply-transformations model-type)))
