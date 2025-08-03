(ns clj-money.db.datomic.util-test
  (:require [clojure.test :refer [deftest is]]
            [clj-money.db.datomic.util :as util]))

(deftest turn-a-map-into-datums
  (is (= [[:db/add 101 :entity/name "Personal"]]
         (util/->datums {:id 101
                         :entity/name "Personal"}))
      "Given properties are returned as additions")
  (is (= [[:db/add 201 :account/entity 101]]
         (util/->datums {:id 201
                         :account/entity {:id 101}}))
      "Model refs are converted to simple ID values")
  (is (= [[:db/add 201 :account/name "Checking"]
          [:db/retract 201 :account/balance]]
         (util/->datums {:id 201
                         :account/name "Checking"
                         :account/balance nil}))
      "Nil values are returned as retractions"))
