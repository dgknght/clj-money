(ns clj-money.util-test
  (:require #?(:clj [clojure.test :refer [deftest is are]]
               :cljs [cljs.test :refer [deftest is are]])
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [clj-money.util :as util]))

(deftest convert-nominal-comparatives-to-symbolic
  (let [date (t/local-date 2015 1 1)
        other-date (t/local-date 2015 1 31)]
    (is (= {:start-on [:> date]}
           (util/symbolic-comparatives {:start-after date}
                                       :start)))
    (is (= {:transaction-date [:> date]}
           (util/symbolic-comparatives {:transaction-date-after date}
                                       :transaction-date))
        "Keys that end with -date don't receive -on or -at")
    (is (= {:transaction-date [:between date other-date]}
           (util/symbolic-comparatives {:transaction-date-on-or-after date
                                        :transaction-date-on-or-before other-date}
                                       :transaction-date)))
    (is (= {:start-on [:between date other-date]}
           (util/symbolic-comparatives {:start-on-or-after date
                                        :start-on-or-before other-date}
                                       :start-on)))))

(deftest convert-symbolic-comparatives-to-nominal
  (let [date (t/local-date 2015 1 1)
        other-date (t/local-date 2015 1 31)]
    (are [m k expected] (= expected (util/nominal-comparatives m k))
         {:start-on date}
         :start
         {:start-on date}

         {:transaction-date date}
         :transaction-date
         {:transaction-date date}

         {:start-on [:> date]}
         :start
         {:start-after date}

         {:transaction-date [:> date]}
         :transaction-date
         {:transaction-date-after date}

         {:transaction-date [:between date other-date]}
         :transaction-date
         {:transaction-date-on-or-after date
          :transaction-date-on-or-before other-date}

         {:start-on [:between date other-date]}
         :start-on
         {:start-on-or-after date
          :start-on-or-before other-date})))

(deftest get-the-earliest-date
  (let [d1 (t/local-date 2020 3 2)
        d2 (t/local-date 2020 2 27)]
    (is (= d2 (util/earliest d1 d2)))))

(deftest extract-a-qualifier
  (is (= "user" (util/qualifier {:user/name "John"}))
      "A single qualifier is taken directly")
  (is (= "user" (util/qualifier {:id 101
                                 :user/name "John"}))
      "Nil namespaces are ignored")
  (is (thrown-with-msg?
        #?(:clj AssertionError
           :cljs js/Error)
        #"more than one keyword namespace"
        (util/qualifier {:user/name "John"
                         :address/line-1 "1234 Main St"}))))

(deftest qualify-map-keys
  (is (= {:entity/name "Personal"}
         (util/qualify-keys {:name "Personal"} :entity))
      "Unqualified keys are qualified with the model type")
  (is (= {:db/id "x"}
         (util/qualify-keys {:db/id "x"} :entity))
      "Qualified keys are left as-is")
  (is (= {:id 101
          :user/name "John"}
         (util/qualify-keys {:id 101 :name "John"}
                           :user
                           :ignore #{:id}))
      "Keys can be explicitly ignored"))

(deftest compare-two-models-for-equality
  (is (util/model= {:id 101}
                   {:id 101})
      "Two maps with the same :id attribute are equal")
  (is (util/model= {:id 101}
                   {:id 101}
                   {:id 101})
      "Three maps with the same :id attribute are equal")
  (is (util/model= {:id 101 :account/name "Checking"}
                   {:id 101})
      "A full model map is equal to a simplified model ref if the :id attribute is the same")
  (is (not (util/model= {:id 101}
                        {:id 102}))
      "Two maps with different :id attributes are not equal"))

(deftest convert-something-into-a-model-ref
  (is (= {:id 101}
         (util/->model-ref {:id 101 :account/name "Checking"}))
      "A full model is simplified")
  (is (= {:id 101}
         (util/->model-ref {:id 101}))
      "A simple model ref is returned as-is")
  (is (= {:id 101}
         (util/->model-ref 101))
      "A naked ID is wrapped in a map"))

(deftest reassembly-an-entity-with-children
  (is (= [#:transaction{:description "Kroger"
                        :items [#:transaction-item{:account {:id :checking}
                                                   :action :debit
                                                   :quantity 100M}
                                #:transaction-item{:account {:id :groceries}
                                                   :action :credit
                                                   :quantity 100M}]}]
         (util/reconstruct {:parent? :transaction/description
                            :child? :transaction-item/account
                            :children-key :transaction/items}
                           [#:transaction{:description "Kroger"}
                            #:transaction-item{:account {:id :checking}
                                               :action :debit
                                               :quantity 100M}
                            #:transaction-item{:account {:id :groceries}
                                               :action :credit
                                               :quantity 100M}]))
      "One transaction receives all of the items")
  (is (= [#:transaction{:description "Kroger"
                        :items [#:transaction-item{:account {:id :checking}
                                                   :action :debit
                                                   :quantity 100M}
                                #:transaction-item{:account {:id :groceries}
                                                   :action :credit
                                                   :quantity 100M}]}
          #:transaction{:description "Landlord"
                        :items [#:transaction-item{:account {:id :checking}
                                                   :action :debit
                                                   :quantity 1000M}
                                #:transaction-item{:account {:id :rent}
                                                   :action :credit
                                                   :quantity 1000M}]}]
         (util/reconstruct {:parent? :transaction/description
                            :child? :transaction-item/account
                            :children-key :transaction/items}
                           [#:transaction{:description "Kroger"}
                            #:transaction-item{:account {:id :checking}
                                               :action :debit
                                               :quantity 100M}
                            #:transaction-item{:account {:id :groceries}
                                               :action :credit
                                               :quantity 100M}
                            #:transaction{:description "Landlord"}
                            #:transaction-item{:account {:id :checking}
                                               :action :debit
                                               :quantity 1000M}
                            #:transaction-item{:account {:id :rent}
                                               :action :credit
                                               :quantity 1000M}
                            #:entity{:name "Personal"}]))
      "Each transaction receives the items until another transaction or the end of the list is encountered"))
