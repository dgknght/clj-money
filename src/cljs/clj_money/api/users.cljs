(ns clj-money.api.users
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [clj-money.api :as api]))

(defn me
  [success-fn error-fn]
  (go (let [response (<! (http/get "/api/users/me"
                                   (api/append-auth {:headers {"Content-Type" "application/json"
                                                               "Accept" "application/json"}})))]
         (if (= 200 (:status response))
           (success-fn (:body response))
           (do
             (.log js/console (prn-str {::me (:body response)}))
             (error-fn (:body response)))))))
