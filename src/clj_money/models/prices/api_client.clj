(ns clj-money.models.prices.api-client
  (:refer-clojure :exclude [update])
  (:require [clj-http.client :as client]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clojure.string :refer [blank?]]
            [cheshire.core :refer :all]
            [clj-time.format :as tf]
            [clj-time.coerce :as tc]))

(def ^:private url-format
  "http://dev.markitondemand.com/MODApis/Api/v2/Quote/json?symbol=%s")

(def ^:private key-map
  {:LastPrice :price
   :Timestamp :trade-date})


(def ^:private timestamp-pattern
  #"^(\w+) (\w+) (\d+) (\d{2}:\d{2}:\d{2}) \w{3}(\S+) (\d{4})$")

(defn- parse-timestamp
  [timestamp]
  (when-not (blank? timestamp)
    (when-let [match (re-matches timestamp-pattern timestamp)]
      ; Thu, 18 May 2017 22:11:54 +0000
      (let [new-string (format "%s, %s %s %s %s %s"
                               (get match 1)
                               (get match 3)
                               (get match 2)
                               (get match 6)
                               (get match 4)
                               (get match 5))]
        (->> new-string
             (tf/parse (tf/formatters :rfc822))
             (tc/to-local-date))))))

(def ^:private transformations
  [{:key :trade-date
    :fn #(parse-timestamp %)}
   {:key :price
    :fn bigdec}])

(defn- perform-transformations
  [price-map]
  (reduce (fn [result {k :key f :fn}]
            (update-in result [k] f))
          price-map
          transformations))

(defn fetch
  [{symbol :symbol}]
  (-> (format url-format symbol)
      (client/get {:content-type :json :as :json})
      :body
      (select-keys (keys key-map))
      (rename-keys key-map)
      perform-transformations))
