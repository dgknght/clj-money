(ns clj-money.api.scheduled-transactions
  (:refer-clojure :exclude [update])
  (:require [cljs.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.web :refer [serialize-date]]
            [clj-money.api :as api :refer [handle-ex]]
            [clj-money.state :refer [current-entity]]))

(defn- path
  [& segments]
  (let [[sched-tran extras] (if (map? (first segments))
                              [(first segments)
                               (rest segments)]
                              [nil segments])]
    (if sched-tran
      (apply api/path
             :scheduled-transactions
             sched-tran
             extras)
      (apply api/path
             :entities
             @current-entity
             :scheduled-transactions
             extras))))

(defn search
  ([xf] (search {} xf))
  ([criteria xf]
   (api/get (path)
            criteria
            {:post-xf xf
             :handle-ex (handle-ex "Unable to retrieve the scheduled transactions: %s")})))

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
  [sched-tran xf]
  (api/post (path)
            (serialize sched-tran)
            {:post-xf xf
             :handle-ex (handle-ex "Unable to create the scheduled transaction: %s")}))

(defn update
  [sched-tran xf]
  (api/patch (path sched-tran)
             (serialize sched-tran)
             {:post-xf xf
              :handle-ex (handle-ex "Unable to update the scheduled transaction: %s")}))

(defn save
  [sched-tran xf]
  (if (:id sched-tran)
    (update sched-tran xf)
    (create sched-tran xf)))

(defn realize
  [& args]
  (let [[sched-tran xf] (if (= 1 (count args))
                          (cons nil args)
                          args)
        path (if sched-tran
               (path sched-tran :realize)
               (path :realize))]
    (api/post  path
              {:post-xf xf
               :handle-ex (handle-ex "Unble to realize the scheduled transaction(s): %s")})))
