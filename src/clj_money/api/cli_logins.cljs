(ns clj-money.api.cli-logins
  (:require [clojure.string :as string]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn approve
  "Approves the command-line app's pending sign-in with the given code, for
  the signed-in user."
  [user-code & {:as opts}]
  (api/post (api/path :cli-logins
                      (js/encodeURIComponent (string/trim user-code))
                      :approve)
            {}
            (add-error-handler opts "Unable to approve the sign-in: %s")))
