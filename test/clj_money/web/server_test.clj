(ns clj-money.web.server-test
  (:require [clojure.test :refer [deftest is]]
            [integrant.core :as ig]
            [clj-money.web :as-alias web]
            [clj-money.web.server]))

(deftest start-and-stop-the-web-server
  (let [server (ig/init-key ::web/server
                            {:handler (constantly {:status 200 :body "OK"})
                             :port 0})
        port (.getLocalPort (first (.getConnectors server)))]
    (try
      (is (.isStarted server)
          "The server is started when initialized")
      (is (= "OK" (slurp (str "http://localhost:" port "/")))
          "The server serves requests with the handler")
      (finally
        (ig/halt-key! ::web/server server)))
    (is (.isStopped server)
        "The server is stopped when halted")))
