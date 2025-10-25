(ns clj-money.api.scheduled-transactions
  (:refer-clojure :exclude [update])
  (:require [cljs.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.dates :as dates]
            [clj-money.util :as util]
            [clj-money.comparatives :as comparatives]
            [clj-money.entities.schema :as schema]
            [clj-money.api :as api :refer [add-error-handler]]
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

(defmulti ^:private serialize-date-param type)

(defmethod serialize-date-param ::util/vector
  [[oper & vs]]
  (apply vector oper (map dates/serialize-local-date vs)))

(defmethod serialize-date-param ::util/string
  [d]
  (dates/serialize-local-date d))

(defn select
  [criteria & {:as opts}]
  (api/get (path)
           (-> criteria
               (update-in-if [:scheduled-transaction/start-date] serialize-date-param)
               (update-in-if [:scheduled-transaction/end-date] serialize-date-param)
               comparatives/nominalize)
           (add-error-handler
             opts
             "Unable to retrieve the scheduled transactions: %s")))

(defn create
  [sched-tran opts]
  (api/post (path)
            sched-tran
            (add-error-handler
              opts
              "Unable to create the scheduled transaction: %s")))

(defn update
  [sched-tran opts]
  (api/patch (path sched-tran)
             sched-tran
             (add-error-handler
               opts
               "Unable to update the scheduled transaction: %s")))

(defn save
  [sched-tran & {:as opts}]
  (let [f (if (:id sched-tran)
            update
            create)]
    (-> sched-tran
        (schema/prune :scheduled-transaction)
        (f opts))))

(defn realize
  [& args]
  (let [[sched-tran & {:as opts}] (if (map? (first args))
                                    args
                                    (cons nil args))
        path (if sched-tran
               (path sched-tran :realize)
               (path :realize))]
    (api/post  path
              {}
              (add-error-handler
                opts
                "Unble to realize the scheduled transaction(s): %s"))))
