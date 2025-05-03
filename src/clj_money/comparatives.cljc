(ns clj-money.comparatives
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clj-money.util :as util]
            [clj-money.dates :as dates]))

(defn- includes-time?
  [d]
  (dates/local-date-time? d))

(defn nominative-variations
  [key-base]
  (let [str-ns (namespace key-base)
        str-key (name key-base)]
    (map #(keyword str-ns (str str-key %))
         [""
          "-before"
          "-on-or-before"
          "-at-or-before"
          "-after"
          "-on-or-after"
          "-at-or-after"])))

(defn- parse-nominal-key
  [k]
  (if (keyword? k)
    (when-let [parsed (parse-nominal-key (name k))]
      (assoc parsed :namespace (namespace k)))
    (when-let [match (re-find #"^(.+?)(-on-or)?-(before|after)$" k)]
      {:base (nth match 1)
       :inclusive? (not (not (nth match 2)))
       :comparison (nth match 3)})))

(defn- parse-symbolic-key
  [k]
  (if (keyword? k)
    (when-let [parsed (parse-symbolic-key (name k))]
      (assoc parsed :namespace (namespace k)))
    (when-let [match (re-find #"^(.+?)(-(on|at))?$" k)]
      {:base (nth match 1)
       :suffix (nth match 2)})))

(defn- symbolic-key-value
  [{:keys [base inclusive? comparison namespace]} v]
  (let [final-key (keyword
                    namespace
                    (str base
                         (when-not (string/ends-with? base "-date")
                           (if (includes-time? v) "-at" "-on"))))
        oper (if inclusive?
               (if (= "before" comparison)
                 :<=
                 :>=)
               (if (= "before" comparison)
                 :<
                 :>))]
    [final-key [oper v]]))

(defn- merge-opers
  [vs]
  (if (= 1 (count vs))
    (first vs)
    (let [oper (case (->> vs (map first) set)
                 #{:< :>}   :<between>
                 #{:<= :>}  :<between
                 #{:< :>=}  :between>
                 #{:<= :>=} :between
                 #{nil}     :between)
          vals (into {} vs)]
      [oper
       ((some-fn :> :>=) vals)
       ((some-fn :< :<=) vals)])))

(defmulti ^:private nominal-key-values
  (fn [_k v]
    (first v)))

(defmethod nominal-key-values :>
  [{:keys [base namespace]} [_ v]]
  [[(keyword namespace
             (str base "-after"))
    v]])

(defmethod nominal-key-values :>=
  [{:keys [base namespace]} [_ v]]
  [[(keyword namespace
             (str base "-on-or-after"))
    v]])

(defmethod nominal-key-values :<
  [{:keys [base namespace]} [_ v]]
  [[(keyword namespace
             (str base "-before"))
    v]])

(defmethod nominal-key-values :<=
  [{:keys [base namespace]} [_ v]]
  [[(keyword namespace
             (str base "-on-or-before"))
    v]])

(defmethod nominal-key-values :between
  [{:keys [base namespace]} [_ start end]]
  [[(keyword namespace
             (str base "-on-or-before"))
    end]
   [(keyword namespace
             (str base "-on-or-after"))
    start]])

(defmethod nominal-key-values :<between
  [{:keys [base namespace]} [_ start end]]
  [[(keyword namespace
             (str base "-on-or-before"))
    end]
   [(keyword namespace
             (str base "-after"))
    start]])

(defmethod nominal-key-values :between>
  [{:keys [base namespace]} [_ start end]]
  [[(keyword namespace
             (str base "-before"))
    end]
   [(keyword namespace
             (str base "-on-or-after"))
    start]])

(defmethod nominal-key-values :<between>
  [{:keys [base namespace]} [_ start end]]
  [[(keyword namespace
             (str base "-before"))
    end]
   [(keyword namespace
             (str base "-after"))
    start]])

(def ^:private comparative-value?
  (every-pred vector?
              (comp keyword? first)))

(defn nominalize
  "Accepts a map and a key base and converts values with attributes
  that match the key base from symbolic comparatives into nominal
  comparatives.

  (nominalize {:end-on [:> some-date]}) => {:end-after some-date}

  (nominalize {:end-date [:> some-date]}) => {:end-date-after some-date}

  (nominalize {:end-at [:> some-date-time]}) => {:end-after some-date-time}"
  [m]
  (with-meta
    (->> m
         (mapcat (fn [[k v :as entry]]
                   (if (comparative-value? v)
                     (nominal-key-values (parse-symbolic-key k)
                                         v)
                     [entry])))
         (into {}))
    (meta m)))

(defn symbolize
  "Accepts a map with comparative keys and updates the
  values with symbolic operators.

  (symbolize {:end-after some-date}) => {:end-on [:> some-date]}

  (symbolize {:end-date-after some-date}) => {:end-date [:> some-date]}

  (symbolize {:end-after some-date-time}) => {:end-at [:> some-date]}
  "
  [m]
  (with-meta
    (->> m
         (map (fn [[k v :as entry]]
                (if-let [parsed (parse-nominal-key k)]
                  (symbolic-key-value parsed v)
                  entry)))
         (util/group-by first second)
         (map #(update-in % [1] merge-opers))
         (into {}))
    (meta m)))
