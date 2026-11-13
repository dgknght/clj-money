(ns clj-money.nav-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [clj-money.nav :as nav]))

(def ^:private items
  [{:id :accounts}
   {:id :reports}
   {:id :search}
   {:id :receipts :group :activity}
   {:id :review :group :activity :badge 2 :badge-class "text-bg-warning"}
   {:id :scheduled :group :activity :badge 1 :badge-class "text-bg-info"}
   {:id :commodities :group :setup :active? true}
   {:id :users :group :setup}])

(deftest group-the-navbar-items
  (let [result (nav/navbar-items items)]
    (is (= [:accounts :reports :activity :setup]
           (map :id result))
        "Each group takes the place of its first member, and search is left out")
    (testing "a group"
      (let [[_ _ activity setup] result]
        (is (= "Activity" (:label activity)))
        (is (= [:receipts :review :scheduled]
               (map :id (:children activity)))
            "The group's members are its children")
        (is (= 3 (:badge activity))
            "The badge totals its members' badges")
        (is (= "text-bg-warning" (:badge-class activity))
            "The badge has the class of its first member's badge")
        (is (not (:active? activity))
            "A group without an active member is not active")
        (is (:active? setup)
            "A group with an active member is active")
        (is (nil? (:badge setup))
            "A group without badged members has no badge")))))

(deftest section-the-items
  (is (= [[nil [:accounts :reports :search]]
          ["Activity" [:receipts :review :scheduled]]
          ["Setup" [:commodities :users]]]
         (map (fn [[label members]]
                [label (map :id members)])
              (nav/sections items)))
      "The ungrouped items come first, without a heading")
  (is (= [["Setup" [:users]]]
         (map (fn [[label members]]
                [label (map :id members)])
              (nav/sections [{:id :users :group :setup}])))
      "Empty sections are left out"))
