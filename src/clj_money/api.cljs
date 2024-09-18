(ns clj-money.api
  (:require [dgknght.app-lib.notifications :as notify]
            [clj-money.state :refer [-busy busy?]]))

(defn handle-ex
  "Given a message format string and optional args, adds a notification
  in the event of an API error, passing the error message as the first
  argument to the format function."
  [msg & args]
  (fn [e]
    (when @busy? (-busy))
    (.dir js/console e)
    (apply notify/dangerf msg (.-message e) args)))
