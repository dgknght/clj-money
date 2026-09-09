(ns clj-money.test-helpers-test
  (:require [clojure.test :refer [deftest is]]
            [clj-money.test-helpers :refer [include-strategy?]]))

(def ^:private strategies
  {:sql {:a 1} :datomic-peer {:b 2} :datomic-client {:c 3}})

(deftest include-strategy-with-only-scopes-to-the-given-strategies
  (is (= [:datomic-peer]
         (->> strategies
              (filter (include-strategy? {:only [:datomic-peer]}))
              (map first)))))

(deftest include-strategy-with-except-excludes-the-given-strategies
  (is (= #{:sql :datomic-client}
         (->> strategies
              (filter (include-strategy? {:except [:datomic-peer]}))
              (map first)
              set))))

(deftest include-strategy-with-neither-only-nor-except-includes-everything
  (is (= (set (keys strategies))
         (->> strategies
              (filter (include-strategy? {}))
              (map first)
              set))))
