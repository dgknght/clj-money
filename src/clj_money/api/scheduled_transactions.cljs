(ns clj-money.api.scheduled-transactions
  (:refer-clojure :exclude [update])
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.web :refer [serialize-date
                                         unserialize-date]]
            [dgknght.app-lib.api-async :as api]
            [clj-money.api :refer [handle-ex]]
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

(defn- transform
  [xf]
  (comp (api/apply-fn after-read)
        xf))

(defn search
  ([xf] (search {} xf))
  ([criteria xf]
   (api/get (path)
            criteria
            {:transform (transform xf)
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
            {:transform (transform xf)
             :handle-ex (handle-ex "Unable to create the scheduled transaction: %s")}))

(defn update
  [sched-tran xf]
  (api/patch (path sched-tran)
             (serialize sched-tran)
             {:transform (transform xf)
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
              {:transform (comp (api/apply-fn trans/after-read)
                                xf)
               :handle-ex (handle-ex "Unble to realize the scheduled transaction(s): %s")})))
