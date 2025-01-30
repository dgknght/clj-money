(ns clj-money.commodities
  (:require [clojure.string :as string]))

(defn- match-commodity?
  [term]
  (let [t (string/lower-case term)]
    (some-fn (comp #(string/includes? % t)
                   string/lower-case
                   :commodity/name)
             (comp #(string/includes? % t)
                   string/lower-case
                   :commodity/symbol))))

(defn search
  ([commodities]
   (fn [term]
     (search term commodities)))
  ([term commodities]
   (filter (match-commodity? term)
           commodities)))
