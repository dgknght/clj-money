(ns clj-money.api
  (:require [clojure.tools.logging :as log]
            [clojure.stacktrace :refer [print-stack-trace]]))

(defn log-error
  [error message]
  (log/errorf "%s\n%s"
              message
              (with-out-str (print-stack-trace error))))
