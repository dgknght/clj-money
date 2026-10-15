(ns clj-money.components-test
  (:require [cljs.test :refer [deftest is testing]]
            [cljs.core.async :as a]
            [clj-money.components :as comps]))

(def ^:private unreconciled
  [{:id 1} {:id 2}])

(deftest append-a-chunk-from-the-active-loader
  (let [loader (a/chan)]
    (is (= [{:id 1} {:id 2} {:id 3}]
           (:items (comps/append-chunk {:item-loader loader
                                        :items unreconciled}
                                       loader
                                       [{:id 3}])))
        "The items are appended to the list")
    (is (= [{:id 3}]
           (:items (comps/append-chunk {:item-loader loader}
                                       loader
                                       [{:id 3}])))
        "The items start the list when there is none")))

(deftest ignore-a-chunk-from-a-stopped-loader
  (testing "a chunk that arrives after the unreconciled items replaced the list"
    (is (= unreconciled
           (:items (comps/append-chunk {:items unreconciled}
                                       (a/chan)
                                       [{:id 1} {:id 2}])))
        "The items are not duplicated"))
  (testing "a chunk from a loader that has been superseded"
    (is (= unreconciled
           (:items (comps/append-chunk {:item-loader (a/chan)
                                        :items unreconciled}
                                       (a/chan)
                                       [{:id 1}])))
        "The items are not duplicated")))
