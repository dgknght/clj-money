(ns clj-money.api.transaction-exports
  (:require [clojure.core.async :as a]
            [clojure.string :as string]
            [cljs-http.client :as http]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.notifications :as notify]
            [clj-money.dates :refer [serialize-local-date]]
            [clj-money.state :refer [auth-token]]
            [clj-money.api :as api]))

(defn- ->query
  [{:keys [since-zero-balance? start-date end-date fields]}]
  (map->query-string
    (cond-> {:fields (string/join "," (map name fields))}
      since-zero-balance? (assoc :since-zero-balance true)
      (and start-date
           (not since-zero-balance?)) (assoc :start-date (serialize-local-date start-date))
      end-date (assoc :end-date (serialize-local-date end-date)))))

(defn- file-name
  [{:keys [headers]}]
  (or (some->> (get headers "content-disposition")
               (re-find #"filename=\"([^\"]+)\"")
               second)
      "transactions.csv"))

(defn export
  "Retrieves the transactions of the account as CSV and passes the response
  body, as a js/Blob, and its file name to on-success.

  options:
  :since-zero-balance? - Start after the account last had a zero balance
  :start-date          - The first date to include, unless starting after a
                         zero balance
  :end-date            - The last date to include
  :fields              - The fields to include, e.g. #{:transaction-date
                         :amount}"
  [account options & {:keys [on-success callback]}]
  (a/go
    (let [{:keys [success status] :as res}
          (a/<! (http/get (str (api/path :accounts
                                         account
                                         :transaction-items
                                         :export)
                               "?"
                               (->query options))
                          {:with-credentials? false
                           :response-type :blob
                           :headers {"Authorization" (str "Bearer " @auth-token)}}))]
      (if success
        (on-success (:body res) (file-name res))
        (notify/dangerf "Unable to export the transactions (status %s)." status))
      (when callback (callback)))))
