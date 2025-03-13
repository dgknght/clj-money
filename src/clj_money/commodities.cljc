(ns clj-money.commodities
  (:require [clj-money.util :as util]))

(defn search
  ([commodities]
   (fn [term]
     (search term commodities)))
  ([term commodities]
   (filter (util/match? term :commodity/name :commodity/symbol)
           commodities)))

(defn description
  [{:commodity/keys [name symbol]}]
  (str name " (" symbol ")"))
