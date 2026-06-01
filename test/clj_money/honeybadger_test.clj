(ns clj-money.honeybadger-test
  (:require [clojure.test :refer [deftest testing is]]
            [clj-http.client :as http]
            [clj-money.config :as config]
            [clj-money.honeybadger :as honeybadger]))

(def ^:private test-error
  (Exception. "something went wrong"))

(deftest notify-sends-to-honeybadger-when-key-is-configured
  (testing "when the api key is configured"
    (let [calls (atom [])]
      (with-redefs [http/post (fn [url opts] (swap! calls conj {:url url :opts opts}))
                    config/env (fn [k]
                                 (when (= :honeybadger-api-key k)
                                   "test-api-key"))]
        (let [f (honeybadger/notify test-error)]
          (when f @f))
        (is (= 1 (count @calls))
            "One POST request is made")
        (let [{:keys [url opts]} (first @calls)]
          (is (= "https://api.honeybadger.io/v1/notices" url)
              "Posts to the HoneyBadger notices endpoint")
          (is (= "test-api-key" (get-in opts [:headers "X-API-Key"]))
              "Sends the API key header"))))))

(deftest notify-is-noop-without-api-key
  (testing "when the api key is not configured"
    (let [calls (atom [])]
      (with-redefs [http/post (fn [url opts] (swap! calls conj {:url url :opts opts}))
                    config/env (constantly nil)]
        (honeybadger/notify test-error)
        (is (empty? @calls)
            "No HTTP calls are made")))))
