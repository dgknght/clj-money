(ns clj-money.commodities
  (:require #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            [clj-money.util :as util]))

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

(defn matches-search?
  [term]
  (if (< 2 (count term))
    (let [pattern (re-pattern (str "(?i)" term))]
      (fn [commodity]
        (some #(re-find pattern (% commodity))
              [:commodity/name
               :commodity/symbol])))
    (constantly true)))

(defn has-shares?
  [{:lot/keys [shares-owned]}]
  (< 0M shares-owned))
