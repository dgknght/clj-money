(ns clj-money.find-in-chunks
  (:refer-clojure :exclude [find])
  (:require [clj-time.core :as t]
            [clj-time.coerce :as tc]
            [clj-money.util :refer [desc-periodic-seq]]))


; For finding batches of records in partitioned tables in multiple queries
; spanning multiple dates in order to avoid many one-record queries

(defn- fetch-records
  [{:keys [fetch-fn] :as opts} xf]
  {:pre [(:fetch-fn opts)]}
  (completing
    (fn [{:keys [ids] :as acc} date]
      (let [records (fetch-fn ids date)]
        (if (seq records)
          (xf acc records)
          (xf acc))))))

(defn- integrate-records
  [{:keys [id-fn
           find-one-fn
           transform-fn]
    :or {transform-fn identity}
    :as opts}
   xf]
  {:pre [(:id-fn opts) (:find-one-fn opts)]}

  (completing
    (fn [acc fetched]
      (xf
        (update-in acc
                   [:results]
                   (fn [records]
                     (->> fetched
                          (group-by id-fn)
                          (map #(update-in % [1] (comp transform-fn
                                                       find-one-fn)))
                          (into records))))
        fetched))))

(defn- remove-satisfied
  [{:keys [id-fn] :as opts} xf]
  {:pre [(:id-fn opts)]}

  (completing
    (fn [acc fetched]
      (xf (update-in acc
                     [:ids]
                     #(apply disj % (map id-fn
                                         fetched)))
          fetched))))

(defn find
  "Given a list of ids, find a list of matching values from the database,
  looking one period at a time.

  Parameters
    ids - a list of ids for which records are to be retrieved
    options -
      start-date:    The date at which to start the descending sequence of date queries
      time-step:     The time between each call to fetch-fn
      fetch-fn:      a function used to fetch the records, accepting a list of ids and a date
      transform-fn:  a function applied to each record to get the desired result to map to the target id
      earliest-date: the date at which to stop looking for records
      id-fn:         a function which returns the id that is the search target when applyed to the a result record from the search
      find-one-fn:   a function used to select the record to be used if more than one record is found for the time period"
  [ids
   {:keys [earliest-date start-date time-step]
    :or {time-step (t/months 1)}
    :as opts}]
  {:pre [(:earliest-date opts)]}

  (let [earliest (tc/to-date-time earliest-date)
        final (->> (desc-periodic-seq (tc/to-date-time start-date)
                                      time-step)
                   (take-while #(or (= earliest %)
                                    (t/before? earliest %)))
                   (transduce (comp (partial fetch-records opts)
                                    (partial integrate-records opts)
                                    (partial remove-satisfied opts))
                              (fn [acc _]
                                (if (empty? (:ids acc))
                                  (reduced acc)
                                  acc))
                              {:results {}
                               :ids ids}))]
    (:results final)))
