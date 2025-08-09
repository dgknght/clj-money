(ns clj-money.util-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [clj-money.dates :as dates]
            [clj-money.util :as util]))

(deftest model-typing
  (testing "Retrieved the type of a model"
    (is (= :user (util/model-type {:user/name "John"}))
        "A model type is derived from the keyword namespace")
    (is (= :user (util/model-type {:id 101
                                   :user/name "John"}))
        "A model type is derived from the keyword namespace if a non-namespace keyword is present")
    (is (= :user (util/model-type {:id 101
                                   :user/first-name "John"
                                   :user/last-name "Doe"
                                   :parent/first-name "Jane"}))
        "A model type is derived from the most frequently occurring keyword namespace if more than one is present")
    (is (= :user (util/model-type ^{:clj-money/model-type :user} {:id 101}))
        "A model type is read from meta data, if present"))
  (testing "Setting the type of a model"
    (is (= :account (util/model-type (util/model-type {} :account)))
        "A model type can be set excplictly")
    (let [f (util/model-type :account)]
      (is (= :account (util/model-type (f {}))))
      "A fn that sets the model type is returned when given a keyword")
    (let [source-model (util/model-type {}  :account)
          target-model (util/model-type {} source-model)]
      (is (= :account (util/model-type target-model)))
      "The model type can be set from another model"))
  (testing "Testing the type of a model"
    (is (util/model-type? {:entity/name "Personal"} :entity))
    (is (not (util/model-type? {:entity/name "Personal"} :account)))
    (let [is-entity? (util/model-type? :entity)]
      (is (is-entity? {:entity/name "Personal"}))
      (is (not (is-entity? {:account/name "Checking"}))))))

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
  (is (= {:util/id "x"}
         (util/qualify-keys {:util/id "x"} :entity))
      "Qualified keys are left as-is")
  (is (= {:id 101
          :user/name "John"}
         (util/qualify-keys {:id 101 :name "John"}
                           :user
                           :ignore #{:id}))
      "Keys can be explicitly ignored"))

(deftest compare-models-for-equality
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
      "Two maps with different :id attributes are not equal")
  (is (not (util/model= {:id 101 :account/name "Checking"}
                        {:id 101 :entity/name "Personal"}))
      "Two maps with different model types are not equal"))

(deftest compare-maps-for-id-equality
  (is (util/id= {:id 101}
                {:id 101})
      "Two maps with the same :id attribute are equal")
  (is (util/id= {:id 101}
                {:id 101}
                {:id 101})
      "Three maps with the same :id attribute are equal")
  (is (util/id= {:id 101 :account/name "Checking"}
                {:id 101})
      "A full model map is equal to a simplified model ref if the :id attribute is the same")
  (is (not (util/id= {:id 101}
                     {:id 102}))
      "Two maps with different :id attributes are not equal")
  (is (util/id= {:id 101 :account/name "Checking"}
                {:id 101 :entity/name "Personal"})
      "Two maps with different model types but equal :id values are equal"))

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
                                                   :quantity 1000M}]}
          #:entity {:name "Personal"}]
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

(deftest identity-a-model-ref
  (is (util/model-ref? {:id 101})
      "A  map with only an :id attribute is a model ref")
  (is (not (util/model-ref? 101))
      "A naked ID is not a model ref")
  (is (not (util/model-ref? {:id 101 :account/name "Checking"}))
      "A full model is not a model ref"))

(deftest cachify-a-function
  (let [calls (atom [])
        f (util/cache-fn
            (fn [x]
              (swap! calls conj x)
              ({:one 1
                :two 2} x)))]
    (is (= 1 (f :one))
        "The specified value is returned the first time")
    (is (= 1 (count @calls))
        "The original function is called the first time")
    (is (= 1 (f :one))
        "The specified value is returned the second time")
    (is (= 1 (count @calls))
        "The original function is not called the second time")))

(deftest identity-a-temporary-id
  (is (util/temp-id? (util/temp-id))
      "The result of calling temp-id is a temporary id")
  (is (not (util/temp-id? 101))
      "An integer is not a temp id")
  (is (not (util/temp-id? "101"))
      "A string is not a temp id")
  (is (not (util/temp-id? (random-uuid)))))

(deftest detect-the-presence-of-a-value
  (testing "strings"
    (is (util/present? "A")
        "A string that is not empty is present")
    (is (not (util/blank? "A"))
        "A string that is not empty is not blank")
    (is (not (util/present? ""))
        "An empty string is not present")
    (is (util/blank? "")
        "An empty string is blank")
    (is (nil? (util/presence ""))
        "The presence of an empty string is nil"))
  (testing "nil"
    (is (not (util/present? nil))
        "Nil is not present")
    (is (nil? (util/presence nil))
        "The presence of nil is nil")
    (is (util/blank? nil)
        "Nil is blank"))
  (testing "vectors"
    (is (not (util/present? []))
        "An empty vector is not present")
    (is (util/blank? [])
        "An empty vector is blank")
    (is (util/present? ["A"])
        "An vector with a non-blank value is present")
    (is (not (util/blank? ["A"]))
        "An vector with a non-blank value is not blank"))
  (testing "lists"
    (is (not (util/present? '()))
        "An empty vector is not present")
    (is (util/blank? '())
        "An empty vector is blank")
    (is (util/present? '("A"))
        "A vector with a non-blank value is present")
    (is (not (util/blank? '("A")))
        "A vector with a non-blank value is not blank"))
  (testing "maps"
    (is (util/present? {:one 1})
        "A map with at least one key is present")
    (is (not (util/present? {}))
        "A map with no keys is not present")))

(deftest use-a-present-value-or-default
  (is (= "A" (util/presence-or "" "A"))
      "A blank value is replaced with the default")
  (is (= "A" (util/presence-or "A" "B"))
      "A present value is returned"))

(deftest update-a-collection-with-a-model
    (is (= [{:id 2 :user/name "Jane"}
            {:id 1 :user/name "John"}]
           (util/upsert-into {:id 2 :user/name "Jane"}
                             {:sort-key :user/name}
                             [{:id 1 :user/name "John"}]))
        "A new item is inserted")
    (is (= [{:id 2 :user/name "Jane"}
            {:id 1 :user/name "John"}]
           (util/upsert-into {:id 2 :user/name "Jane"}
                             {:sort-key :user/name}
                             [{:id 2 :user/name "Joan"}
                              {:id 1 :user/name "John"}]))
        "An existing item is replaed"))

(deftest render-a-simplified-model
  (is (= {:account/name "Checking"}
         (util/simplify {:account/name "Checking"
                         :account/balance 100M}))
      "A map is simplified")
  (is (= [{:account/name "Checking"}]
         (util/simplify '({:account/name "Checking"
                           :account/balance 100M})))
      "A sequence of maps is simplified")
  (is (= {:unknown-key 123}
         (util/simplify {:unknown-key 123}))
      "A map with no recognized keys is returned as-is")
  (is (= 123
         (util/simplify 123))
      "An unrecognized type is returned as-is")
  (is (= {:id 123}
         (util/simplify {:id 123}))
      "A model ref is returned as-is")
  (let [f (util/simplify :include [:account/balance])]
    (is (= {:account/name "Checking"
            :account/balance 100M}
           (f {:account/name "Checking"
               :account/balance 100M
               :account/type :asset}))
        "When only options are specified, a simplifying function is returned")))

(deftest make-a-keyword-safe-for-urls
  (is (= :account_name (util/url-safe-keyword :account/name))
      "A keyword with a namespace is separated with an underscore")
  (is (= :name (util/url-safe-keyword :name))
      "A keyword without a namespace is returned as-is"))

(deftest convert-a-url-safe-keyword-into-a-regular-keyword
  (is (= :account/name (util/<-url-safe-keyword :account_name))
      "An underscore is used to separate the namespace from the name")
  (is (= :name (util/<-url-safe-keyword :name))
      "A value without underscore returned left as-is"))

(deftest group-and-transform-a-sequence
    (is (= {:one [1 11]
            :two [2]}
           (util/group-by
               first
               second
               [[:one 1]
                [:one 11]
                [:two 2]]))))

(deftest turn-a-sequence-into-a-range
  (is (= [1 4]
         (util/->range [2 4 3 1]))
      "Integers can be processed")
  (is (= [(dates/local-date "2020-01-01")
          (dates/local-date "2020-12-01")]
         (util/->range [(dates/local-date "2020-01-01")
                        (dates/local-date "2020-12-01")
                        (dates/local-date "2020-05-01")]
                       :compare t/before?))
      "Integers can be processed"))

(deftest remove-nils-from-a-model
  (testing "one level"
    (is (= {:present :here}
           (util/remove-nils {:present :here
                              :absent nil}))))
  (testing "collection attribute"
    (is (= {:present :here
            :others [{:one 1}
                     {:two 2}]}
           (util/remove-nils {:present :here
                              :others [{:one 1
                                        :two nil}
                                       {:one nil
                                        :two 2}]})))))

(deftest locate-nils-in-a-model
  (is (= [[:absent]]
         (util/locate-nils {:present :here
                            :absent nil})))
  (is (= [[:others 0 :two]
          [:others 1 :one]]
         (util/locate-nils {:present :here
                            :others [{:one 1
                                      :two nil}
                                     {:one nil
                                      :two 2}]}))))

(deftest ensure-a-model-has-an-id
  (is (= {:id 1} (util/+id {} (constantly 1)))
      "An :id attribute is added if none is present")
  (is (= {:id 2} (util/+id {:id 2} (constantly 1)))
      "An :id attribute is left as-is if it is already present"))

(deftest rename-keys-nested-in-a-data-structure
  (is (= [{:db/id 1
           :user/name "John"}
          {:db/id 2
           :user/name "Jane"}]
         (util/deep-rename-keys
           [{:id 1
             :user/name "John"}
            {:id 2
             :user/name "Jane"}]
           {:id :db/id}))))

(deftest apply-sort-rule
  (let [d1 (t/local-date 2001 1 1)
        d2 (t/local-date 2002 1 1)
        d3 (t/local-date 2004 1 1)
        items [{:v 2 :d d1 :s "carrot"}
               {:v 1 :d d3 :s "banana"}
               {:v 3 :d d2 :s "apple"}]]
    (testing "Ascending sort on one string field, implicit direction"
      (is (= [{:s "apple"}
              {:s "banana"}
              {:s "carrot"}]
             (map #(select-keys % [:s])
                  (util/apply-sort {:order-by [:s]}
                                  items)))))
    (testing "Ascending sort on one integer field, implicit direction"
      (is (= items
             (util/apply-sort {} items))))
    (testing "Ascending sort on one integer field, implicit direction"
      (is (= [{:v 1}
              {:v 2}
              {:v 3}]
             (map #(select-keys % [:v])
                  (util/apply-sort {:order-by [:v]}
                                  items)))))
    (testing "Ascending sort on one date field, implicit direction"
      (is (= [{:d d1}
              {:d d2}
              {:d d3}]
             (map #(select-keys % [:d])
                  (util/apply-sort {:order-by [:d]}
                                  items)))))
    (testing "Ascending sort on one integer field, explicit direction"
      (is (= [{:v 1}
              {:v 2}
              {:v 3}]
             (map #(select-keys % [:v])
                  (util/apply-sort {:order-by [[:v :asc]]}
                                  items)))))
    (testing "Descending sort on one integer field"
      (is (= [{:v 3}
              {:v 2}
              {:v 1}]
             (map #(select-keys % [:v])
                  (util/apply-sort {:order-by [[:v :desc]]}
                                  items)))))
    (testing "Multi-field sort"
      (is (= [{:v 1 :d d3}
              {:v 2 :d d1}
              {:v 2 :d d2}
              {:v 3 :d d2}]
             (map #(select-keys % [:v :d])
                  (util/apply-sort {:order-by [:v :d]}
                                  (conj items {:v 2 :d d2}))))))))
