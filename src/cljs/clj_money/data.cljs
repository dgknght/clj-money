(ns clj-money.data
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [clojure.walk :refer [keywordize-keys]]
            [cognitect.transit :as transit]))

(defn get-entities
  [callback]
  (go (let [response (<! (http/get "/api/entities"))]
        (if (= 200 (:status response))
          (let [reader (transit/reader :json {:keywordize-keys true})]
            (-> (transit/read reader (:body response))
                keywordize-keys
                callback))
          (.log js/console "Unable to get the entities from the service" (:body response))))))
