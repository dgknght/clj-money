(ns clj-money.web.system-test
  (:require [clojure.test :refer [deftest is]]
            [clj-money.web.system :as system]))

(deftest get-a-component-from-a-request
  (let [handler (system/wrap-system (fn [req]
                                      (system/component req :services))
                                    {:services ::services})]
    (is (= ::services (handler {}))
        "The component assoc'ed to the request is returned"))
  (is (nil? (system/component {} :services))
      "Nil is returned when the components are not assoc'ed to the request"))
