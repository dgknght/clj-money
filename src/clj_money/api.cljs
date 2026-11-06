(ns clj-money.api
  (:refer-clojure :exclude [get])
  (:require [cljs.pprint :refer [pprint]]
            [cljs.reader :as reader :include-macros true]
            [lambdaisland.uri :as uri]
            [reagent.cookies :as cookies]
            [dgknght.app-lib.notifications :as notify]
            [dgknght.app-lib.api-3 :as api]
            [clj-money.dates :as dates]
            [clj-money.decimal :as decimal]
            [clj-money.state :as state :refer [auth-token]]
            [clj-money.return-path :as return-path]))

(reader/register-tag-parser! 'clj-money/local-date-time clj-money.dates/local-date-time)
(reader/register-tag-parser! 'clj-money/local-date clj-money.dates/local-date)
(reader/register-tag-parser! 'clj-money/instant clj-money.dates/unserialize-instant)
(reader/register-tag-parser! 'clj-money/decimal clj-money.decimal/d)

(def default-options
  {:encoding :edn})

(defn path
  [& segments]
  {:pre [(every? identity segments)]}
  (->> segments
       (map #(if (map? %) (:id %) %))
       (apply api/path)))

(def ^:private error-msg
  (some-fn (comp :message ex-data)
           ex-message))

(defn handle-ex
  "Given a message format string and optional args, adds a notification
  in the event of an API error, passing the error message as the first
  argument to the format function."
  [msg & args]
  (fn [e]
    (pprint {::error e
             ::ex-data (ex-data e)})
    (apply notify/dangerf msg (error-msg e) args)
    e))

(defn add-error-handler
  "Given a map of API options, if no :on-error is provider, include
  one that uses the specified message format."
  [{:keys [on-error] :as opts} msg & args]
  (if on-error
    opts
    (assoc opts :on-error (apply handle-ex msg args))))

(defn- end-session
  "When the server rejects the auth token (e.g., because it has
  expired), signs the user out and sends them to the sign in page, to come
  back once they sign in again"
  [{:keys [status] :as res}]
  (when (and (= 401 status) @auth-token)
    (return-path/remember! (str js/location.pathname js/location.search))
    (state/logout)
    (cookies/remove! :auth-token)
    (.assign js/location "/login"))
  res)

(defn- authenticate
  [options]
  (-> default-options
      (merge options)
      (update :pre-xf #(if % (comp (map end-session) %) (map end-session)))
      (assoc :oauth-token @auth-token)))

(defn- append-query
  [url query]
  {:pre [(string? url)
         (or (nil? query)
             (map? query))]}
  (if (seq query)
    (-> (uri/uri url)
        (assoc :query (uri/map->query-string query))
        str)
    url))

(defn get
  ([url options] (get url {} options))
  ([url query options]
   (api/get (append-query url query)
            (authenticate options))))

(defn post
  ([url options] (post url {} options))
  ([url payload options]
   (api/post url
             payload
             (authenticate options))))

(defn patch
  [url payload options]
  (api/patch url
             payload
             (authenticate options)))

(defn delete
  [url options]
  (api/delete url
              (authenticate options)))
