(ns clj-money.web.handler-test
  (:require [clojure.test :refer [deftest is]]
            [integrant.core :as ig]
            [ring.mock.request :as req]
            [dgknght.app-lib.test-assertions]
            [clj-money.config :refer [env]]
            [clj-money.db :as db]
            [clj-money.images :as img]
            [clj-money.progress :as prog]
            [clj-money.web :as-alias web]
            [clj-money.web.handler :as handler]))

(deftest build-the-handler-at-system-initialization
  (let [handler (ig/init-key ::web/handler {:env env})]
    (is (http-success? (handler (req/request :get "/images/logo.svg")))
        "The handler serves requests")))

(deftest the-session-secret-is-required-at-system-initialization
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"SESSION_SECRET"
                        (ig/init-key ::web/handler
                                     {:env (dissoc env :session-secret)}))))

(deftest bind-the-component-values-for-each-request
  (let [bindings (atom nil)
        wrap-components @#'handler/wrap-components
        handler (wrap-components (fn [_]
                                   (reset! bindings
                                           [db/*storage*
                                            img/*storage*
                                            prog/*tracker-factory*])
                                   {:status 200})
                                 {:storage ::storage
                                  :image-storage ::image-storage
                                  :tracker-factory ::tracker-factory})]
    (handler {})
    (is (= [::storage ::image-storage ::tracker-factory]
           @bindings)
        "The component values are bound while the request is handled")))

(deftest keep-existing-bindings-for-missing-components
  (let [bindings (atom nil)
        wrap-components @#'handler/wrap-components
        handler (wrap-components (fn [_]
                                   (reset! bindings
                                           [db/*storage*
                                            img/*storage*
                                            prog/*tracker-factory*])
                                   {:status 200})
                                 {})]
    (binding [db/*storage* ::outer-storage]
      (handler {}))
    (is (= [::outer-storage nil nil]
           @bindings)
        "The existing bindings are used")))
