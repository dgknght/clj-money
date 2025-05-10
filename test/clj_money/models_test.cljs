(ns clj-money.models-test
  (:require [cljs.test :refer [deftest is]]
            [clj-money.models :as models]))

(deftest prune-a-model-for-submission
  (is (= {:id 1
          :commodity/symbol "AAPL"}
         (models/prune {:id 1
                        :commodity/symbol "AAPL"
                        :commodity/updated-at "2020-01-01T00:00:00.000Z"
                        :entity/name "Personal"}
                       :commodity))
      "Intrinsic model attributes are returned, the rest are omitted."))
