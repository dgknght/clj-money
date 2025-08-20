(ns clj-money.progress.redis
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [taoensso.carmine :as car]
            [clj-money.progress :as prog]))

(defonce conn-pool (car/connection-pool {}))

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
  (car/wcar redis-opts
            (car/set (build-key opts process-key :total)
                     expected-count)))

(defn- increment*
  [{:keys [redis-opts] :as opts} process-key completed-count]
  (car/wcar redis-opts
            (car/incrby (build-key opts process-key :completed)
                        completed-count)))

(defn- parse-key
  [{:keys [prefix root]}]
  (let [ignore (->> [prefix root "progress"]
                    (map stringify)
                    (filter identity)
                    set)]
    (fn [k]
      (let [segments (->> (string/split k #":")
                          (remove ignore)
                          (map keyword))]
        [(vec (butlast segments))
         (last segments)]))))

(defn- get*
  [{:keys [redis-opts] :as opts}]
  (let [parse (parse-key opts)
        pattern (build-key opts "*")
        keys (car/wcar redis-opts
                       (car/keys pattern))]
    (->> keys
         (map #(vector (parse %) %))
         (map #(update-in % [1] (fn [k]
                                  (car/wcar redis-opts
                                            (car/get k)))))
         (reduce (fn [m [k v]]
                   (assoc-in m k v))
                 {}))))

(defmethod prog/reify-tracker ::prog/redis
  [{:keys [redis-config] :as opts} root-key]
  {:pre [(:redis-config opts)]}
  (let [opts* (-> opts
                  (dissoc ::prog/strategy
                          :redis-config)
                  (assoc :redis-opts {:pool conn-pool
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
        (get* opts*)))))
