(ns clj-money.api
  (:refer-clojure :exclude [get])
  (:require [cljs.pprint :refer [pprint]]
            [cljs.reader :as reader :include-macros true]
            [lambdaisland.uri :as uri]
            [dgknght.app-lib.notifications :as notify]
            [dgknght.app-lib.api-3 :as api]
            [clj-money.dates :as dates]
            [clj-money.decimal :as decimal]
            [clj-money.state :refer [-busy busy? auth-token]]))

(reader/add-data-readers
  {'local-date-time dates/local-date-time
   'local-date dates/local-date
   'bigdec decimal/d})

(def default-options
  {:encoding :edn})

(defn path
  [& segments]
  (->> segments
       (map #(if (map? %) (:id %) %))
       (apply api/path)))

(defn handle-ex
  "Given a message format string and optional args, adds a notification
  in the event of an API error, passing the error message as the first
  argument to the format function."
  [msg & args]
  (fn [e]
    (when @busy? (-busy))
    (.dir js/console e)
    (apply notify/dangerf msg (.-message e) args)))

(defn- append-query
  [url query]
  (if (seq? query)
    (-> (uri/uri url)
        (assoc :query (uri/map->query-string query))
        str)
    url))

(defn get
  ([url options] (get url {} options))
  ([url query options]
   (api/get (append-query url query)
            (-> default-options
                (merge options)
                (assoc :oauth-token @auth-token)))))

(defn post
  ([url options] (post url {} options))
  ([url payload options]
   (api/post url
             payload
             (-> default-options
                 (merge options)
                 (assoc :oauth-token @auth-token)))))

(defn patch
  [url payload options]
  (api/patch url
             payload
             (-> default-options
                 (merge options)
                 (assoc :oauth-token @auth-token))))

(defn delete
  [url options]
  (api/delete url
              (-> default-options
                  (merge options)
                  (assoc :oauth-token @auth-token))))
