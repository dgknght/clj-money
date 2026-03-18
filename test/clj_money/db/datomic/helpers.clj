(ns clj-money.db.datomic.helpers
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.dates :as dates]
            [clj-money.db.datomic :refer [q]]))

(def ^:private specs
  {:account {:query '[:find ?a ?name ?commodity ?quantity ?value
                      :where [?a :account/name ?name]
                             [?a :account/quantity ?quantity]
                             [?a :account/value ?value]
                             [?a :account/commodity ?c]
                             [?c :commodity/symbol ?commodity]]
             :headers ["ID" "Name" "Commodity" "Quantity" "Value"]
             :col-widths [16 20 10 10 10]
             :sort-fn second}
   :commodity {:query '[:find ?c ?name ?symbol ?type
                        :where [?c :commodity/name ?name]
                               [?c :commodity/symbol ?symbol]
                               [?c :commodity/type ?type]]
               :headers["ID" "Name" "Symbol" "Type"]
               :col-widths [16 20 10 10]
               :sort-fn second}
   :transaction-item {:query '[:find ?i ?date ?index ?action ?parent ?account ?quantity ?balance
                               :where [?i :transaction-item/index ?index]
                                      [?i :transaction-item/quantity ?quantity]
                                      [?i :transaction-item/action ?action]
                                      [?i :transaction-item/balance ?balance]
                                      [?i :transaction-item/account ?a]
                                      [?a :account/name ?account]
                                      [?t :transaction/items ?i]
                                      [?t :transaction/transaction-date ?date]
                                      (or-join [?a ?parent]
                                               (and [?a :account/parent ?p]
                                                    [?p :account/name ?parent])
                                               (and [(missing? $ ?a :account/parent)]
                                                    [(ground "") ?parent]))]
                      :headers ["ID"
                                "Date"
                                "Index"
                                "Action"
                                "Parent"
                                "Account"
                                "Quantity"
                                "Balance"]
                      :col-widths [16 10 6 7 20 20 19 19]
                      :sort-fn (juxt #(nth % 4)
                                     #(nth % 5)
                                     #(nth % 2))
                      :transform (fn [r]
                                   (update-in r
                                              [1]
                                              (comp dates/format-local-date
                                                    dates/->local-date)))}})

(defn- print-vals
  [vals]
  (doseq [val vals]
    (print val))
  (print "\n"))

(defn- print-row
  [vals widths]
  (->> vals
       (interleave widths)
       (partition 2)
       (map (fn [[w f]]
              (format (str "%-" w "s") f)))
       (interpose " ")
       print-vals))

(defn- print-separator
  [widths]
  (->> widths
       (map #(apply str (repeat % "-")))
       (interpose " ")
       print-vals))

(defn print-entities
  [entity-type]
  (assert (specs entity-type) (str "No spec found for entity type:" entity-type))
  (println "")
  (println (name entity-type))
  (let [{:keys [col-widths
                headers
                query
                sort-fn
                transform]
         :or {transform identity}} (specs entity-type)]
    (print-row headers col-widths)
    (print-separator col-widths)
    (doseq [row (->> (q query) (map transform) (sort-by sort-fn))]
      (print-row row col-widths)))
  (println ""))
