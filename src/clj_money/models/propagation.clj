(ns clj-money.models.propagation
  (:require [clojure.core.async :as a]
            [clj-money.util :as util]
            [clj-money.models :as models]))

; A map of priority number to sets of propagation fns
(def ^:private full-propagations (atom {}))

(defn propagate-all
  "Performs a full propagation for all models in the system.

  Model namespaces register a function with add-full-propagation so that when
  this function is called, all of the mode-specific propagations will be executed."
  ([]
   (->> @full-propagations
        (sort-by first)
        (mapcat second)
        (map #(%))
        doall))
  ([entity]
   (->> @full-propagations
        (sort-by first)
        (mapcat second)
        (reduce (fn [entity f]
                  (if-let [updated (->> (f entity)
                                        (filter (util/model-type? :entity))
                                        first)]
                    updated
                    entity))
                entity)
        doall)))

(defn add-full-propagation
  "Registers a function that will be executed with propagate-all is invoked.

  The function must have two arities: One that accepts an entity as the single argument,
  and one that accepts no arguments (and processes all entities)"
  [f & {:keys [priority] :or {priority 10}}]
  (swap! full-propagations update-in [priority] (fnil conj #{}) f))

(defmulti propagate #(->> %
                          (filter identity)
                          (some util/model-type)))
(defmethod propagate :default [[_before _after]] [])

(def propagation-xf
  (comp (map propagate)
        (remove empty?)
        (map #(when (seq %)
                (models/put-many %)))))

(defn propagation-chan
  "Returns a channel that accepts change tuples (before, after) (from the out-chan
   of either put, put-many, delete, or delete-many) and propagates changes to
   any other models affected by the change."
  []
  (a/chan 1 propagation-xf))

(defn +propagation
  [f & {:keys [combine-with] :or {combine-with concat}}]
  (fn [model]
    (let [out-chan (propagation-chan)
          copy-chan (a/chan)
          propagations (atom [])
          _ (a/go-loop [x (a/<! out-chan)]
              (when x
                (swap! propagations concat x)
                (recur (a/<! out-chan))))
          result (f model
                    :out-chan out-chan
                    :copy-chan copy-chan
                    :close-chan? false)]
      (a/alts!! [copy-chan (a/timeout 5000)])
      (a/close! out-chan)
      (combine-with result @propagations))))

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmacro with-propagation
  [bindings & body]
  `(let [f# (fn* [~(first bindings) ~(second bindings)] ~@body)
         out# (propagation-chan)
         copy# (a/chan)
         prim-result# (f# out# copy#)]
     (a/go (a/alts! [out# (a/timeout 5000)]))
     (a/<!! copy#)
     (a/close! out#)
     prim-result#))

(defn- act-and-propagate
  [model f {:as opts}]
  (let [out-chan (propagation-chan)
        result (apply f
                      model
                      (mapcat identity
                              (assoc opts
                                     :out-chan out-chan)))]
    (cons result
          (a/<!! out-chan))))

(defn put-and-propagate
  [model & {:as opts}]
  (act-and-propagate model models/put (mapcat identity opts)))

(defn delete-and-propagate
  [model & {:as opts}]
  (act-and-propagate model models/delete (mapcat identity opts)))
