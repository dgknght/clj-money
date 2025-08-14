(ns clj-money.models.propagation
  (:require [clojure.core.async :as a]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clj-money.util :as util]
            [clj-money.models :as models]))

; A map of priority number to sets of propagation fns
(def ^:private full-propagations (atom {}))

(defn- propagation-fn [opts]
  (->> @full-propagations
       (sort-by first >)
       (mapcat second)
       (map (fn [f] #(f % opts)))
       (apply comp)))

(defn propagate-all
  "Performs a full propagation for all models in the system.

  Model namespaces register a function with add-full-propagation so that when
  this function is called, all of the mode-specific propagations will be executed."
  ([opts]
   (doseq [entity (models/select (util/model-type {} :entity))]
     (propagate-all entity opts)))
  ([entity opts]
   (log/debugf "[propagation] start entity %s"
               (:entity/name entity))
   (let [f (propagation-fn opts)]
     (try
       (f entity)
       (log/infof "[propagation] finished entity %s"
                  (:entity/name entity))
       (catch Exception e
         (log/error e "[propagation] Unable to finish the full propagation"))))))

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

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmacro with-propagation
  "Propagate puts that occur during the block. The two bindings must be
  passed to models/put-many and are:
    - out-chan: a channel for receiving changes emitted from the put
    - ctrl-chan: a channel for receiving notifications about the stopping and starting of the puts"
  [bindings & body]
  `(let [f# (fn* [~(first bindings) ~(second bindings)] ~@body)
         out# (a/chan)
         res# (a/transduce propagation-xf
                           concat
                           []
                           out#)
         ctrl# (a/chan)
         pending# (atom 0)
         prim-complete# (atom false)
         sec-complete# (a/promise-chan)
         _# (a/go-loop [msg# (a/<! ctrl#)]
                      (when msg#
                        (let [count# (swap! pending#
                                            (case msg# :start inc :finish dec))]
                          (when (and @prim-complete#
                                     (= 0 count#))
                            (a/>! sec-complete# :done)))
                        (recur (a/<! ctrl#))))
         prim-result# (f# out# ctrl#)
         _# (reset! prim-complete# true)
         combine# (if (sequential? prim-result#)
                    concat
                    cons)]
     (a/alts!! [sec-complete# (a/timeout 5000)])
     (a/close! out#)
     (combine# prim-result# (a/<!! res#))))

(defn +propagation
  [f]
  (fn [model]
    (with-propagation [out-chan ctrl-chan]
      (f model
         :out-chan out-chan
         :ctrl-chan ctrl-chan
         :close-chan? false))))

(defn- act-and-propagate
  [model f {:as opts :keys [return-all?] :or {return-all? false}}]
  (let [out-chan (a/chan 1 propagation-xf)
        result (apply f
                      model
                      (mapcat identity
                              (assoc opts
                                     :out-chan out-chan)))
        propagations (a/<!! out-chan)]
    (if return-all?
      (cons result propagations)
      result)))

(defn put-and-propagate
  [model & {:as opts}]
  (act-and-propagate model models/put (mapcat identity opts)))

(defn delete-and-propagate
  [model & {:as opts}]
  (act-and-propagate model models/delete (mapcat identity opts)))
