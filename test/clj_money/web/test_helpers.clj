(ns clj-money.web.test-helpers
  (:require [clojure.test :refer [is]]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [ring.util.response :as res]))

(defn- valid-response?
  [{:keys [body]}]
  (string? body))

(defn assert-successful
  [res]
  {:pre [(valid-response? res)]}

  (let [s (:status res)]
    (is (and (<= 200 s)
             (> 300 s))
        (str "Expected successful response, but got " s))))

(defn assert-successful-create
  [res]
  {:pre [(valid-response? res)]}

  (let [s (:status res)]
    (is (= 201 s)
        (str "Expected creation success response, but got " s))))

(defn assert-successful-no-content
  [res]
  {:pre [(valid-response? res)]}

  (let [s (:status res)]
    (is (= 204 s)
        (str "Expected successful response, but got " s))))

(defn assert-created
  [res]
  {:pre [(valid-response? res)]}

  (is (= 201 (:status res)) "The response has a 201 status"))

(defn assert-redirects-to
  [res url]
  {:pre [(valid-response? res)]}

  (is (= 302 (:status res)) "The response is a redirect")
  (is (= url (get-in res [:headers "Location"]))
      (format "The response redirects to %s" url)))

(defn assert-bad-request
  [res]
  {:pre [(valid-response? res)]}

  (is (= 400 (:status res)) "The response has the correct status"))

(defn assert-unauthorized
  [res]
  {:pre [(valid-response? res)]}

  (is (= 401 (:status res)) "The response has the correct status")
  (is (= {:message "unauthorized"} (json/parse-string (:body res) true))))

(defn assert-forbidden
  [res]
  {:pre [(valid-response? res)]}

  (is (= 403 (:status res)) "The response has the correct status")
  (is (= {:message "forbidden"} (json/parse-string (:body res) true))))

(defn assert-not-found
  [res]
  {:pre [(valid-response? res)]}

  (is (= 404 (:status res)) "The response has the correct status")
  (is (= {:message "not found"} (json/parse-string (:body res) true))))

(defn json-response
  [body]
  (-> body
      json/generate-string
      res/response
      (res/content-type "application/json")))

(defn matching-response
  [{:keys [uri response]} _method requested-uri _options]
  (when (= uri requested-uri)
    response))

(defn find-response
  [responses method uri options]
  (or (some #(matching-response % method uri options)
            responses)
      (throw (ex-info "Unable to find a matching response for mock"
                      {:method method
                       :uri uri
                       :options options
                       :responses responses}))))

(defn mock-response
  [history responses method]
  (fn [uri options]
    (swap! history update-in [method] conj {:uri uri
                                            :options options})
    (find-response responses method uri options)))

(defmacro with-http-mocks
  [bindings responses & body]
  `(let [~(first bindings) (atom {:get [] :post []})]
     (with-redefs [http/post (mock-response ~(first bindings) ~responses :post)
                   http/get (mock-response ~(first bindings) ~responses :get)]
       ~@body)))
