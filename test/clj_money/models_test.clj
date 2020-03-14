(ns clj-money.models-test
  (:require [clojure.test :refer [deftest is]]
            [clj-money.models :as models]))

(deftest tag-a-model
  (let [model {:name "Personal"}
        tagged (models/tag model ::models/entity)
        retrieved (models/tag tagged)]
    (is (= ::models/entity retrieved))))
