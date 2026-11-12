(ns clj-money.web.handler-test
  (:require [clojure.test :refer [deftest is]]
            [integrant.core :as ig]
            [ring.mock.request :as req]
            [dgknght.app-lib.test-assertions]
            [clj-money.config :refer [env]]
            [clj-money.db :as db]
            [clj-money.entities :as entities]
            [clj-money.images :as img]
            [clj-money.progress :as prog]
            [clj-money.web :as-alias web]
            [clj-money.web.handler :as handler]))

(deftest build-the-handler-at-system-initialization
  (let [handler (ig/init-key ::web/handler {:env env})]
    (is (http-success? (handler (req/request :get "/images/logo.svg")))
        "The handler serves requests")))

(deftest respond-with-not-found-for-a-missing-static-asset
  (let [handler (ig/init-key ::web/handler {:env env})]
    (is (http-not-found? (handler (req/request :get "/cljs-out/missing.js")))
        "The app's HTML is not served in place of the asset")))

(deftest cache-a-versioned-static-asset-indefinitely
  (let [handler (ig/init-key ::web/handler {:env env})
        res (handler (req/request :get "/js/bootstrap.min.js?v=abc123"))]
    (is (http-success? res)
        "The asset is served")
    (is (= "public, max-age=31536000, immutable"
           (get-in res [:headers "Cache-Control"]))
        "The asset can be cached indefinitely")))

(deftest leave-the-caching-of-an-unversioned-static-asset-alone
  (let [handler (ig/init-key ::web/handler {:env env})
        res (handler (req/request :get "/js/bootstrap.min.js"))]
    (is (http-success? res)
        "The asset is served")
    (is (nil? (get-in res [:headers "Cache-Control"]))
        "No Cache-Control header is set")))

(deftest always-revalidate-the-service-worker
  (let [handler (ig/init-key ::web/handler {:env env})
        res (handler (req/request :get "/service-worker.js"))]
    (is (http-success? res)
        "The service worker is served")
    (is (= "no-cache" (get-in res [:headers "Cache-Control"]))
        "Caches must check with the server before using a stored copy")))

(deftest the-session-secret-is-required-at-system-initialization
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"SESSION_SECRET"
                        (ig/init-key ::web/handler
                                     {:env (dissoc env :session-secret)}))))

(defn- session-cookie
  [site-protocol]
  (let [handler (ig/init-key ::web/handler
                             {:env (assoc env :site-protocol site-protocol)
                              :services {:oauth {:google {:client-id "id"
                                                          :client-secret "secret"}}}})]
    (->> (get-in (handler (req/request :get "/auth/google/start"))
                 [:headers "Set-Cookie"])
         (filter #(re-find #"^ring-session=" %))
         first)))

(deftest mark-the-session-cookie-secure-when-served-over-https
  (let [cookie (session-cookie "https")]
    (is cookie "The session cookie is set")
    (is (re-find #"(?i);\s*Secure" cookie)
        "The cookie is marked Secure")
    (is (re-find #"(?i);\s*HttpOnly" cookie)
        "The cookie is marked HttpOnly")
    (is (re-find #"(?i);\s*SameSite=Lax" cookie)
        "The cookie is marked SameSite=Lax")))

(deftest leave-the-session-cookie-unsecured-when-served-over-http
  (let [cookie (session-cookie "http")]
    (is cookie "The session cookie is set")
    (is (not (re-find #"(?i);\s*Secure" cookie))
        "The cookie is not marked Secure")))

(deftest bind-the-component-values-for-each-request
  (let [bindings (atom nil)
        wrap-components @#'handler/wrap-components
        handler (wrap-components (fn [_]
                                   (reset! bindings
                                           [db/*storage*
                                            entities/*publisher*
                                            img/*storage*
                                            prog/*tracker-factory*])
                                   {:status 200})
                                 {:storage ::storage
                                  :publisher ::publisher
                                  :image-storage ::image-storage
                                  :tracker-factory ::tracker-factory})]
    (handler {})
    (is (= [::storage ::publisher ::image-storage ::tracker-factory]
           @bindings)
        "The component values are bound while the request is handled")))

(deftest keep-existing-bindings-for-missing-components
  (let [bindings (atom nil)
        wrap-components @#'handler/wrap-components
        handler (wrap-components (fn [_]
                                   (reset! bindings
                                           [db/*storage*
                                            entities/*publisher*
                                            img/*storage*
                                            prog/*tracker-factory*])
                                   {:status 200})
                                 {})]
    (binding [db/*storage* ::outer-storage]
      (handler {}))
    (is (= [::outer-storage nil nil nil]
           @bindings)
        "The existing bindings are used")))
