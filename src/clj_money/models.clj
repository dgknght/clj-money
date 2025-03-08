(ns clj-money.models
  (:refer-clojure :exclude [find count update])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :as a]
            [dgknght.app-lib.validation :as v]
            [dgknght.app-lib.models :refer [->id]]
            [clj-money.models :as models]
            [clj-money.json] ; to ensure encoders are registered
            [clj-money.util :as util :refer [model=]]
            [clj-money.db :as db]))

(s/def ::model (s/and map?
                      util/model-type))
(s/def ::puttable (s/or :map       ::model
                        :operation (s/tuple ::db/operation ::model)))
(s/def ::puttables (s/coll-of ::puttable :min-count 1))

; A map of priority number to sets of propagation fns
(def ^:private full-propagations (atom {}))

(defn propagate-all []
  (->> @full-propagations
       (sort-by first)
       (mapcat second)
       (mapv #(%))))

(defn add-full-propagation
  [f & {:keys [priority] :or {priority 10}}]
  (swap! full-propagations update-in [priority] (fnil conj #{}) f))

(def exchanges #{:nyse :nasdaq :amex :otc})

(s/def ::id (some-fn uuid? int? util/temp-id?))
(s/def ::model-ref (s/keys :req-un [::id]))

(defmulti prepare-criteria util/model-type-dispatch)
(defmethod prepare-criteria :default [m] m)

(defmulti before-validation util/model-type-dispatch)
(defmethod before-validation :default [m & _] m)

(defmulti propagate #(->> %
                          (filter identity)
                          (some util/model-type)))
(defmethod propagate :default [[_before _after]] [])

(defmulti before-save util/model-type-dispatch)
(defmethod before-save :default [m & _] m)

(defmulti after-save util/model-type-dispatch)
(defmethod after-save :default [m & _] m)

(defmulti after-read util/model-type-dispatch)
(defmethod after-read :default [m & _] m)

(defmulti before-delete util/model-type-dispatch)
(defmethod before-delete :default [m & _] m)

(defn- validation-key
  [m]
  (keyword "clj-money.models"
           (-> m util/model-type name)))

(defn- validate
  [model]
  (let [validated (v/validate model (validation-key model))]
    (when (seq (::v/errors validated))
      (throw (ex-info "Validation failed" (select-keys validated [::v/errors])))))
  model)

(defn before
  ([model]
   (-> model
       meta
       ::before))
  ([model k]
   (-> model
       before
       k)))

(defn- append-before
  [model]
  (vary-meta model assoc ::before model))

(defn select
  ([criteria] (select criteria {}))
  ([criteria options]
   (map (comp append-before
              #(after-read % options))
        (db/select (db/storage)
                   (prepare-criteria criteria)
                   options))))

(defn count
  [criteria]
  (:record-count (db/select (db/storage) criteria {:count true})))

(defn find-by
  ([criteria] (find-by criteria {}))
  ([criteria options]
   (first (select criteria (assoc options :limit 1)))))

(defn find-many
  [m-or-ids model-type]
  (select (util/model-type {:id [:in (mapv ->id m-or-ids)]}
                         model-type)))

(defn find
  "Find a model by id or by reference map.

  When given one argument:
    - If the argument is a model reference, return the model
    - If the argument is a keyword, a function that will look up models of the specified type
  When given two arguments, look up a model of the spcified type having the specified id"
  ([arg]
   (if (keyword? arg)
     #(find % arg)
     (do
       (assert (:id arg) "The argument must have an id")
       (assert (util/model-type arg) "The argument must have a model type")
       (find (:id arg)
           (keyword (util/model-type arg)))))) ; TODO: can we remove the call to keyword?
  ([id-or-ref model-type]
   {:pre [id-or-ref (keyword? model-type)]}
   (find-by (util/model-type (util/->model-ref id-or-ref)
                           model-type))))

(def ^:private mergeable?
  (every-pred map? :id))

(defn- merge-dupes
  "Given a sequence of models, merge any that have the same id"
  [puttables]
  (loop [input puttables output []]
    (if-let [puttable (first input)]
      (if (mergeable? puttable)
        (let [{dupes true
               others false} (group-by #(model= puttable %)
                                       (rest input))]
          (recur others (conj output (apply merge puttable dupes))))
        (recur (rest input) (conj output puttable)))
      output)))

(defn- duplicate-present?
  [ms]
  (->> ms
       (remove vector?)
       (filter :id)
       (map (juxt util/model-type :id))
       (frequencies)
       (remove (comp #{1} second))
       seq))

(defn- throw-on-duplicate
  [ms]
  (when (duplicate-present? ms)
    (throw (ex-info "Duplicate model found" {:models ms})))
  ms)

(defn- handle-dupes
  [{:keys [on-duplicate] :or {on-duplicate :throw}} ms]
  (case on-duplicate
    :merge-last-wins (merge-dupes ms)
    :throw (throw-on-duplicate ms)
    (throw (ex-info "Invalid on-duplicate value" {:on-duplicate on-duplicate}))))


(defn- dispatch
  "Returns a function that accepts either a naked model or a model wrapped in a
  vector with a db operator in the first position, and applies the function to
  the model, unless it's a delete operation."
  ([f]
   #(dispatch f %))
  ([f x]
   (if (vector? x)
     (let [[oper :as puttable] x]
       (if (= ::db/delete oper)
         puttable
         (update-in puttable [1] f)))
     (f x))))

(defn- ensure-id
  "When saving a new record, make sure we have a temp id"
  [m]
  (if (map? m) ; this could also be a vector like [:delete {:id 123}]
    (update-in m [:id] (fn [id]
                         (or id (util/temp-id))))
    m))

(def ^:private deletion?
  (every-pred vector?
              #(= ::db/delete (first %))))

(defn- emit-changes
  [{:keys [to-save
           result
           out-chan
           copy-chan
           close-chan?]
    :or {close-chan? true}}]
  (when out-chan
    (a/go
      (let [coll (concat
                   (->> result
                        (interleave to-save)
                        (partition 2)
                        (map (fn [[input after]]
                               (if (deletion? input)
                                 [(second input) nil]
                                 [(before input) after])))))
            c (a/onto-chan! out-chan coll close-chan?)]
        (when copy-chan
          (a/pipe c copy-chan))))))

(defn put-many
  "Save a sequence of models to the database, providing lifecycle hooks that
  are dispatched by model type, including: before-validation, before-save,
  after-save, etc.

  Options:
  :on-duplicate - one of :merge-last-wins, :merge-first-wins, or :throw
  :out-chan     - An async channel that when passed, receives change tuples
                  containing before and after versions of each model affected
                  by the operation.
  :copy-chan    - An async channel that receives a message when the out-chan
                  has received all pending results.
  :close-chan?  - A boolean value indicating whether or not the out-chan should be
                  closed automatically once all pending results have been sent."
  ([models] (put-many {} models))
  ([{:as opts
     :keys [storage]}
    models]
   {:pre [(s/valid? ::puttables models)]}

   (let [to-save (->> models
                      (handle-dupes opts)
                      (map (dispatch
                             (comp ensure-id
                                   before-save
                                   validate
                                   before-validation))))
         {:keys [saved]} (db/put (or storage
                                     (db/storage))
                                 to-save)
         result (map (comp append-before
                           after-save
                           #(after-read % {}))
                     saved)]
     (emit-changes (assoc opts
                          :to-save to-save
                          :result result))
     result)))

(defn propagation-chan
  "Returns a channel that will accept before & after output (from the out-chan
   of either put, put-many, delete, or delete-many) and propagates changes to
   any other models affected by the change."
  []
  (a/chan 1 (comp (map propagate)
                  (remove empty?)
                  (map put-many))))

(defn put
  [model & {:as opts}]
  (first (put-many opts [model])))

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
  (act-and-propagate model put (mapcat identity opts)))

(defn delete-many
  ([models] (delete-many {} models))
  ([{:keys [out-chan]} models]
   {:pre [(seq (filter identity models))]}
   (let [result (->> models
                     (map before-delete)
                     (db/delete (db/storage)))]
     (when out-chan
       (a/go
         (->> models
              (map #(vector % nil))
              (a/onto-chan!! out-chan))))
     result)))

(defn delete
  [model & {:as opts}]
  {:pre [model]}
  (delete-many opts [model]))

(defn delete-and-propagate
  [model & {:as opts}]
  (act-and-propagate model delete (mapcat identity opts)))

(defn resolve-ref
  ([model-type]
   (fn [model-or-ref]
     (resolve-ref model-or-ref model-type)))
  ([model-or-ref model-type]
   (if (util/model-ref? model-or-ref)
     (find model-or-ref model-type)
     model-or-ref)))

(defn +propagation
  [f & {:keys [combine-with] :or {combine-with concat}}]
  (fn [model]
    (let [out-chan (propagation-chan)
          copy-chan (a/chan)
          propagations (atom [])
          _ (a/go-loop [x (a/<! out-chan)]
              (swap! propagations concat x)
              (recur (a/<! out-chan)))
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
