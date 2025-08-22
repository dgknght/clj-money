(ns clj-money.progress.redis
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [taoensso.carmine :as car]
            [clj-money.progress :as prog]))

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

(defn- expect*
  [{:keys [redis-opts] :as opts} process-key expected-count]
  (try
    (car/wcar redis-opts
              (car/set (build-key opts :processes process-key :total)
                       expected-count))
    (catch Exception e
      (log/error e "Unable to write the expectation to redis"))))

(defn- increment*
  [{:keys [redis-opts] :as opts} process-key completed-count]
  (try
    (car/wcar redis-opts
              (car/incrby (build-key opts :processes process-key :completed)
                          completed-count))
    (catch Exception e
      (log/error e "Unable to increment the completed count in redis"))))

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
        keys (car/wcar redis-opts
                       (car/keys pattern))
        vals (car/wcar redis-opts
                       (mapv #(car/get %) keys))]
    (assoc (->> vals
                (interleave keys)
                (partition 2)
                (map (comp #(update-in % [0] (parse-key opts))
                           #(update-in % [1] parse-long)
                           vec))
                (reduce (fn [m [k v]]
                          (assoc-in m k v))
                        {}))
           :warnings (car/wcar redis-opts
                               (car/lrange (build-key opts :warnings) 0 -1))
           :finished (= "1"
                        (car/wcar redis-opts
                                  (car/get (build-key opts :finished)))))))

(defn- warn*
  [{:keys [redis-opts] :as opts} msg]
  (try
    (car/wcar redis-opts
              (car/rpush (build-key opts :warnings)
                         msg))
    (catch Exception e
      (log/error e "Unable to record the warning"))))

(defn- fail*
  [{:keys [redis-opts] :as opts} msg]
  (try
    (car/wcar redis-opts
              (car/set (build-key opts :failure-reason)
                       msg))
    (catch Exception e
      (log/error e "Unable to record the warning"))))

(defn- finish*
  [{:keys [redis-opts] :as opts}]
  (try
    (car/wcar redis-opts
              (car/set (build-key opts :finished)
                       "1"))
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
