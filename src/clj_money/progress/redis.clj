(ns clj-money.progress.redis
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [java-time.api :as t]
            [taoensso.carmine :as car]
            [clj-money.dates :as dates]
            [clj-money.progress :as prog]))

(def ex-seconds (.getSeconds (t/duration 90 :days)))

(defn- stringify
  [x]
  (when x
    (cond
      (keyword? x) (name x)
      :else (str x))))

(defn- build-key
  [{:keys [root prefix]} & args]
  (->> args
       (concat (->> [prefix "progress" root]
                    (map stringify)
                    (filter identity)))
       flatten
       (map stringify)
       (string/join ":")))

(defn- proc-key
  [opts process-key & args]
  (apply build-key opts :processes process-key args))

(defn- now []
  (dates/serialize-instant (t/instant)))

(defn- expect*
  [{:keys [redis-opts] :as opts} process-key expected-count]
  (try
    (let [start-key (build-key opts :started-at)
          started-at (car/wcar redis-opts
                               (car/get start-key))]
      (car/wcar redis-opts
                (car/set (proc-key opts process-key :total)
                         expected-count
                         "EX" ex-seconds)
                (when-not started-at
                  (car/set start-key (now)))))
    (catch Exception e
      (log/error e "Unable to write the expectation to redis"))))

(defn- increment*
  [{:keys [redis-opts] :as opts} process-key completed-count]
  (try
    (let [k (proc-key opts process-key :completed)
          now (now)
          [completed
           _
           total
           started-at] (car/wcar redis-opts
                                 (car/incrby k completed-count)
                                 (car/expire k ex-seconds)
                                 (car/get (proc-key opts process-key :total))
                                 (car/get (proc-key opts process-key :started-at)))]
      (when-not started-at
        (car/wcar redis-opts
                  (car/set (proc-key opts process-key :started-at)
                           now
                           "EX" ex-seconds)))
      (when (and total
                 (>= completed (parse-long total)))
        (car/wcar redis-opts
                  (car/set (proc-key opts process-key :completed-at)
                           now
                           "EX" ex-seconds))))
    (catch Exception e
      (log/errorf
        e
        "Unable to increment the completed count by %s in redis for %s"
        completed-count
        process-key))))

(defn- parse-key
  [{:keys [prefix root]}]
  (let [ignore (->> [prefix root "progress"]
                    (map stringify)
                    (filter identity)
                    set)]
    (fn [k]
      (->> (string/split k #":")
           (remove ignore)
           (map keyword)))))

(defn- get*
  [{:keys [redis-opts] :as opts}]
  (let [pattern (build-key opts :processes "*")
        [finished
         warnings
         started-at
         completed-at
         keys] (car/wcar redis-opts
                         (car/get (build-key opts :finished))
                         (car/lrange (build-key opts :warnings) 0 -1)
                         (car/get (build-key opts :started-at))
                         (car/get (build-key opts :completed-at))
                         (car/keys pattern))
        vals (when (seq keys)
               (car/wcar redis-opts
                         (mapv #(car/get %) keys)))]
    (assoc (->> vals
                (interleave keys)
                (partition 2)
                (map (comp #(update-in % [0] (parse-key opts))
                           #(update-in % [1] (fn [x]
                                               (if (and (string? x)
                                                        (re-matches #"\d+" x))
                                                 (parse-long x)
                                                 x)))
                           vec))
                (reduce (fn [m [k v]]
                          (assoc-in m k v))
                        {}))
           :warnings warnings
           :finished (= "1" finished)
           :started-at started-at
           :completed-at completed-at)))

(defn- warn*
  [{:keys [redis-opts] :as opts} msg]
  (try
    (let [k (build-key opts :warnings)]
      (car/wcar redis-opts
                (car/lpush k msg)
                (car/expire k ex-seconds)))
    (catch Exception e
      (log/error e "Unable to record the warning"))))

(defn- fail*
  [{:keys [redis-opts] :as opts} msg]
  (try
    (car/wcar redis-opts
              (car/set (build-key opts :failure-reason)
                       msg
                       "EX" ex-seconds))
    (catch Exception e
      (log/error e "Unable to record the warning"))))

(defn- finish*
  [{:keys [redis-opts] :as opts}]
  (try
    (car/wcar redis-opts
              (car/set (build-key opts :finished)
                       "1"
                       "EX" ex-seconds)
              (car/set (build-key opts :completed-at)
                       (now)
                       "EX" ex-seconds))
    (catch Exception e
      (log/error e "Unable to mark the process as finished"))))

(defmethod prog/reify-tracker ::prog/redis
  [{:keys [redis-config] :as opts} root-key]
  {:pre [(:redis-config opts)]}
  (let [opts* (-> opts
                  (dissoc ::prog/strategy
                          :redis-config)
                  (assoc :redis-opts {:pool {}
                                      :spec redis-config}
                         :root root-key))]
    (reify prog/Tracker
      (expect [_ process-key expected-count]
        (expect* opts* process-key expected-count))
      (increment [_ process-key]
        (increment* opts* process-key 1))
      (increment [_ process-key completed-count]
        (increment* opts* process-key completed-count))
      (get [_]
        (get* opts*))
      (warn [_ msg]
        (warn* opts* msg ))
      (fail [_ msg]
        (fail* opts* msg ))
      (finish [_]
        (finish* opts*)))))
