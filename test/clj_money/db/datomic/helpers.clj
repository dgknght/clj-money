(ns clj-money.db.datomic.helpers
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.db.datomic :refer [q]]))

(def ^:private specs
  {:account {:query '[:find ?a ?name ?quantity ?value
                      :where [?a :account/name ?name]
                             [?a :account/quantity ?quantity]
                             [?a :account/value ?value]]
             :headers ["ID" "Name" "Quantity" "Value"]
             :col-widths [16 20 10 10]}
   :commodity {:query '[:find ?c ?name ?symbol ?type
                        :where [?c :commodity/name ?name]
                               [?c :commodity/symbol ?symbol]
                               [?c :commodity/type ?type]]
               :headers["ID" "Name" "Symbol" "Type"]
               :col-widths [16 20 10 10]}})

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
  (let [{:keys [col-widths headers query]} (specs entity-type)]
    (print-row headers col-widths)
    (print-separator col-widths)
    (doseq [row (sort-by second (q query))]
      (print-row row col-widths)))
  (println ""))
