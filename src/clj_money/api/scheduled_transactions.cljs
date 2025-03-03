(ns clj-money.api.scheduled-transactions
  (:refer-clojure :exclude [update])
  (:require [clj-money.api :as api :refer [add-error-handler]]
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
  [criteria & {:as opts}]
  (api/get (path)
           criteria
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
    (f sched-tran opts)))

(defn realize
  [& args]
  (let [[sched-tran & {:as opts}] (if (map? (first args))
                                    (cons nil args)
                                    args)
        path (if sched-tran
               (path sched-tran :realize)
               (path :realize))]
    (api/post  path
              {}
              (add-error-handler
                opts
                "Unble to realize the scheduled transaction(s): %s"))))
