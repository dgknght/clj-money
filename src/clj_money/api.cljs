(ns clj-money.api
  (:refer-clojure :exclude [get])
  (:require [dgknght.app-lib.notifications :as notify]
            [dgknght.app-lib.api-async :as api]
            [clj-money.state :refer [-busy busy? auth-token]]))

(def apply-fn api/apply-fn)
(def path api/path)

(defn handle-ex
  "Given a message format string and optional args, adds a notification
  in the event of an API error, passing the error message as the first
  argument to the format function."
  [msg & args]
  (fn [e]
    (when @busy? (-busy))
    (.dir js/console e)
    (apply notify/dangerf msg (.-message e) args)))

(defn get
  ([url options] (get url {} options))
  ([url payload options]
   (api/get url
            payload
            (assoc options :oauth-token @auth-token))))

(defn post
  [url payload options]
  (api/post url
            payload
            (assoc options :oauth-token @auth-token)))

(defn patch
  [url payload options]
  (api/patch url
             payload
             (assoc options :oauth-token @auth-token)))

(defn delete
  [url options]
  (api/delete url
              (assoc options :oauth-token @auth-token)))
