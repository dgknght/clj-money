(ns clj-money.formats-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is]])
            [clj-money.formats :as fmts]))

(deftest convert-edn-to-json
  (is (= {:firstName "John"
          :lastName "Doe"
          :_type :user}
         (fmts/edn->json
           {:user/first-name "John"
            :user/last-name "Doe"}))
      "A 1-level map get updated keys")
  (is (= {:name "Personal"
          :settings {:inventoryMethod :fifo
                     :_type :settings}
          :_type :entity}
         (fmts/edn->json
           {:entity/name "Personal"
            :entity/settings {:settings/inventory-method :fifo}}))
      "A nested map also gets updated keys"))

(deftest convert-json-to-edn
  (is (= {:user/first-name "John"
          :user/last-name "Doe"}
         (fmts/json->edn
           {:firstName "John"
            :lastName "Doe"
            :_type :user}))
      "A 1-level map get updated keys")
  (is (= {:entity/name "Personal"
          :entity/settings {:settings/inventory-method :fifo}}
         (fmts/json->edn
           {:name "Personal"
            :settings {:inventoryMethod :fifo
                       :_type :settings}
            :_type :entity}))
      "A nest map also gets updated keys"))
