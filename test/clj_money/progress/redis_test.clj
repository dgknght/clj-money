(ns clj-money.progress.redis-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [clj-money.config :refer [env]]
            [taoensso.carmine :as car]
            [clj-money.progress :as prog]
            [clj-money.progress.redis]))

(defmacro with-redis
  [& body]
  (let [config (get-in env [:progress
                            :strategies
                            :redis
                            :redis-config])]
    `(car/wcar {:pool {}
                :spec ~config}
               ~@body)))

(defn- reset-redis [f]
  (let [prefix (get-in env [:progress
                            :strategies
                            :redis
                            :prefix])
        pattern (if prefix
                  (str prefix ":*")
                  "*")
        keys (with-redis (car/keys pattern))]
    (with-redis (doseq [k keys] (car/del k))))
  (f))

(use-fixtures :each reset-redis)

(defn- track
  [f calls]
  (fn [& args]
    (swap! calls update-in [f] (fnil conj []) args)))

(defmacro with-mocked-redis
  [bindings & body]
  `(let [calls# (atom {})
         f# (fn* [~(first bindings)]
                 ~@body)]
     (with-redefs [car/set (track :set calls#)
                   car/incrby (track :incrby calls#)]
       (f# calls#))))

(def ^:private mock-config
  {::prog/strategy ::prog/redis
   :redis-config {}})

(deftest notify-a-count-of-a-category
  (testing "keyword root key"
    (with-mocked-redis [calls]
      (let [tracker (prog/reify-tracker mock-config
                                        :root)]
        (prog/expect tracker :accounts 123)
        (is (= {:set [["progress:root:accounts:total" 123]]}
               @calls)
            "The expected count is saved at the key for based on the specified category"))))
  (testing "integer root key"
    (with-mocked-redis [calls]
      (let [tracker (prog/reify-tracker mock-config
                                        111)]
        (prog/expect tracker :accounts 123)
        (is (= {:set [["progress:111:accounts:total" 123]]}
               @calls)
            "The expected count is saved at the key for based on the specified category"))))
  (testing "composite process key"
    (with-mocked-redis [calls]
      (let [tracker (prog/reify-tracker mock-config
                                        :root)]
        (prog/expect tracker [:propagation :transactions] 123)
        (is (= {:set [["progress:root:propagation:transactions:total" 123]]}
               @calls)
            "The expected count is saved at the key for based on the specified category"))))
  (testing "with a prefix"
    (with-mocked-redis [calls]
      (let [tracker (prog/reify-tracker (assoc mock-config
                                               :prefix "my-prefix")
                                        :root)]
        (prog/expect tracker [:propagation :transactions] 123)
        (is (= {:set [["my-prefix:progress:root:propagation:transactions:total" 123]]}
               @calls)
            "The expected count is saved at the key for based on the specified category")))))

(deftest increment-the-progress
  (testing "increment by the implicit amount"
    (with-mocked-redis [calls]
      (let [tracker (prog/reify-tracker mock-config
                                        :root)]
        (prog/increment tracker :accounts)
        (is (= {:incrby [["progress:root:accounts:completed" 1]]}
               @calls)
            "The completed count is saved at the key for based on the specified category"))))
  (testing "increment by an explicit amount"
    (with-mocked-redis [calls]
      (let [tracker (prog/reify-tracker mock-config
                                        :root)]
        (prog/increment tracker :accounts 5)
        (is (= {:incrby [["progress:root:accounts:completed" 5]]}
               @calls)
            "The completed count is saved at the key for based on the specified category")))))

(deftest ^:redis round-trip
  (let [tracker (prog/reify-tracker (get-in env [:progress :strategies :redis])
                                    :my-root)]
    (prog/expect tracker :accounts 100)
    (prog/increment tracker :accounts)
    (prog/increment tracker :accounts)
    (is (= {:accounts {:total 100
                       :completed 2}}
           (prog/get tracker)))))
