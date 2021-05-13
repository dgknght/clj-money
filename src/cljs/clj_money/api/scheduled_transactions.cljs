(ns clj-money.api.scheduled-transactions
  (:refer-clojure :exclude [update])
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.web :refer [serialize-date
                                         unserialize-date]]
            [dgknght.app-lib.api :as api]
            [clj-money.state :refer [current-entity]]
            [clj-money.api.transactions :as trans]))

(defn- after-item-read
  [item]
  (update-in item [:action] keyword))

(defn- path
  [& segments]
  (let [[sched-tran extras] (if (map? (first segments))
                              [(first segments)
                               (rest segments)]
                              [nil segments])]
    (if sched-tran
      (apply api/path
             :scheduled-transactions
             (:id sched-tran)
             extras)
      (apply api/path
             :entities
             (:id @current-entity)
             :scheduled-transactions
             extras))))

(defn- after-read
  [sched-tran]
  (-> sched-tran
      (update-in [:interval-type] keyword)
      (update-in-if [:date-spec :days] #(->> %
                                             (map keyword)
                                             set))
      (update-in-if [:date-spec :day] #(if (string? %)
                                         (keyword %)
                                         %))
      (update-in [:start-date] unserialize-date)
      (update-in-if [:end-date] unserialize-date)
      (update-in-if [:last-occurrence] unserialize-date)
      (update-in [:items] #(mapv after-item-read %))))

(defn search
  ([success-fn error-fn] (search {} success-fn error-fn))
  ([criteria success-fn error-fn]
   (api/get (path)
            criteria
            #(success-fn (map after-read %))
            error-fn)))

(defn- serialize
  [sched-tran]
  (-> sched-tran
      (update-in-if [:last-occurrence] serialize-date)
      (update-in-if [:end-date] serialize-date)
      (update-in [:start-date] serialize-date)
      (select-keys [:id
                    :description
                    :interval-type
                    :interval-count
                    :start-date
                    :end-date
                    :date-spec
                    :last-occurrence
                    :items
                    :enabled
                    :memo])))

(defn create
  [sched-tran success-fn error-fn]
  (api/post (path)
            (serialize sched-tran)
            (comp success-fn after-read)
            error-fn))

(defn update
  [sched-tran success-fn error-fn]
  (api/patch (path sched-tran)
             (serialize sched-tran)
             (comp success-fn after-read)
             error-fn))

(defn save
  [sched-tran success-fn error-fn]
  (if (:id sched-tran)
    (update sched-tran success-fn error-fn)
    (create sched-tran success-fn error-fn)))

(defn realize
  [& args]
  (let [[sched-tran success-fn error-fn] (if (= 2 (count args))
                                           (cons nil args)
                                           args)
        path (if sched-tran
               (path sched-tran :realize)
               (path :realize))]
    (api/post  path
              (comp success-fn
                    #(map trans/after-read %))
              error-fn)))
