(ns clj-money.views.util
  (:require [dgknght.app-lib.busy :refer [-busy]]
            [dgknght.app-lib.notifications :refer [dangerf]]))

(defn handle-error
  [state msg]
  (fn [error]
    (-busy state)
    (dangerf msg error)))
