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
   :prices {:keys #{:price/price
                    :price/trade-date}
            :references #{:price/commodity}}})

(defn- apply-transformations
  [m model-type]
  (reduce (fn [model k]
            (update-in-if model [k] util/->model-ref))
          m
          (-> models model-type :references)))

(defn prune
  [m model-type & {:keys [exclude]}]
  (-> m
      (select-keys (cons :id (-> models model-type :keys)))
      (apply dissoc exclude)
      (apply-transformations model-type)))
