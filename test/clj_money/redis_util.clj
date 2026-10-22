(ns clj-money.redis-util
  (:require [taoensso.carmine :as car]
            [clj-money.config :refer [env]]))

(defmacro with-redis
  [& body]
  (let [config (get-in env [:progress
                            :strategies
                            :redis
                            :redis-config])]
    `(car/wcar {:pool {}
                :spec ~config}
               ~@body)))

(defn reset-redis [f]
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

(defn track
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
