(ns clj-money.progress.redis-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [clj-money.config :refer [env]]
            [taoensso.carmine :as car]
            [clj-money.dates :refer [with-fixed-time]]
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
                   car/get (constantly nil)
                   car/incrby (track :incrby calls#)
                   car/rpush (track :rpush calls#)
                   car/lpush (track :rpush calls#)]
       (f# calls#))))

(def ^:private mock-config
  {::prog/strategy ::prog/redis
   :redis-config {}})

(deftest notify-a-count-of-a-category
  (testing "keyword root key"
    (with-mocked-redis [calls]
      (let [tracker (prog/reify-tracker mock-config
                                        :root)]
        (with-fixed-time "2020-01-01T00:00:00.000Z"
          (prog/expect tracker :accounts 123))
        (is (= {:set [["progress:root:processes:accounts:total"
                       123
                       "EX"
                       7776000]
                      ["progress:root:started-at"
                       "2020-01-01T00:00:00Z"
                       "EX"
                       7776000]]}
               @calls)
            "The expected count is saved at the key based on the specified category"))))
  (testing "integer root key"
    (with-mocked-redis [calls]
      (let [tracker (prog/reify-tracker mock-config
                                        111)]
        (with-fixed-time "2020-01-01T00:00:00.000Z"
          (prog/expect tracker :accounts 123))
        (is (= {:set [["progress:111:processes:accounts:total"
                       123
                       "EX"
                       7776000]
                      ["progress:111:started-at"
                       "2020-01-01T00:00:00Z"
                       "EX"
                       7776000]]}
               @calls)
            "The expected count is saved at the key based on the specified category"))))
  (testing "composite process key"
    (with-mocked-redis [calls]
      (let [tracker (prog/reify-tracker mock-config
                                        :root)]
        (with-fixed-time "2020-01-01T00:00:00.000Z"
          (prog/expect tracker [:propagation :transactions] 123))
        (is (= {:set [["progress:root:processes:propagation:transactions:total"
                       123
                       "EX"
                       7776000]
                      ["progress:root:started-at"
                       "2020-01-01T00:00:00Z"
                       "EX"
                       7776000]]}
               @calls)
            "The expected count is saved at the key based on the specified category"))))
  (testing "with a prefix"
    (with-mocked-redis [calls]
      (let [tracker (prog/reify-tracker (assoc mock-config
                                               :prefix "my-prefix")
                                        :root)]
        (with-fixed-time "2020-01-01T00:00:00.000Z"
          (prog/expect tracker [:propagation :transactions] 123))
        (is (= {:set [["my-prefix:progress:root:processes:propagation:transactions:total"
                       123
                       "EX"
                       7776000]
                      ["my-prefix:progress:root:started-at"
                       "2020-01-01T00:00:00Z"
                       "EX"
                       7776000]]}
               @calls)
            "The expected count is saved at the key based on the specified category")))))

(deftest increment-the-progress
  (testing "increment by the implicit amount"
    (with-mocked-redis [calls]
      (let [tracker (prog/reify-tracker mock-config
                                        :root)]
        (with-fixed-time "2020-01-01T00:00:00.000Z"
          (prog/increment tracker :accounts))
        (is (= {:incrby [["progress:root:processes:accounts:completed" 1]]}
               @calls)
            "The completed count is saved at the key based on the specified category"))))
  (testing "increment by an explicit amount"
    (with-mocked-redis [calls]
      (let [tracker (prog/reify-tracker mock-config
                                        :root)]
        (with-fixed-time "2020-01-01T00:00:00.000Z"
          (prog/increment tracker :accounts 5))
        (is (= {:incrby [["progress:root:processes:accounts:completed" 5]]}
               @calls)
            "The completed count is saved at the key based on the specified category")))))

(deftest send-a-notification
  (testing "warning"
    (with-mocked-redis [calls]
      (let [tracker (prog/reify-tracker mock-config
                                        :root)]
        (prog/warn tracker
                   "non-essential task failed")
        (is (= {:rpush [["progress:root:warnings"
                         "non-essential task failed"]]}
               @calls)
            "The message is pushed into the list of warnings"))))
  (testing "fatal"
    (with-mocked-redis [calls]
      (let [tracker (prog/reify-tracker mock-config
                                        :root)]
        (prog/fail tracker
                   "catastrophic failure")
        (is (= {:set [["progress:root:failure-reason"
                       "catastrophic failure"
                       "EX"
                       7776000]]}
               @calls)
            "The message is set as the cause of an abnormal ending")))))

(deftest finalize-a-process
  (with-mocked-redis [calls]
    (let [tracker (prog/reify-tracker mock-config
                                      :root)]
      (with-fixed-time "2020-01-01T00:00:00.000Z"
        (prog/finish tracker))
      (is (= {:set [["progress:root:finished"
                     "1"
                     "EX"
                     7776000]
                    ["progress:root:completed-at"
                     "2020-01-01T00:00:00Z"
                     "EX"
                     7776000]]}
             @calls)
          "The flag is set indicating the process has finished")))) 

(deftest ^:redis round-trip
  (let [tracker (prog/reify-tracker (get-in env [:progress :strategies :redis])
                                    :my-root)]
    (with-fixed-time "2020-01-01T00:00:00.000Z"
      (prog/expect tracker :commodities 2)
      (prog/expect tracker :accounts 100)
      (prog/increment tracker :commodities)
      (prog/increment tracker :commodities)
      (prog/increment tracker :accounts)
      (prog/warn tracker "Something not too bad")
      (prog/finish tracker))
    (is (= {:processes {:accounts {:total 100
                                   :completed 1
                                   :started-at "2020-01-01T00:00:00Z"}
                        :commodities {:total 2
                                      :completed 2
                                      :started-at "2020-01-01T00:00:00Z"
                                      :completed-at "2020-01-01T00:00:00Z"}}
            :warnings ["Something not too bad"]
            :finished true
            :started-at "2020-01-01T00:00:00Z"
            :completed-at "2020-01-01T00:00:00Z"}
           (prog/get tracker)))))
