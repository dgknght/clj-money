(ns clj-money.honeybadger-test
  (:require [clojure.test :refer [deftest testing is]]
            [clj-http.client :as http]
            [clj-money.honeybadger :as honeybadger]))

(def ^:private test-error
  (Exception. "something went wrong"))

(deftest ^:multi-threaded notify-sends-to-honeybadger-when-key-is-configured
  (testing "when the api key is configured"
    (let [calls (atom [])]
      (with-redefs [http/post (fn [url opts] (swap! calls conj {:url url :opts opts}))]
        (honeybadger/notify test-error
                            {:api-key "test-api-key"
                             :environment-name "test"})
        (is (= 1 (count @calls))
            "One POST request is made")
        (let [{:keys [url opts]} (first @calls)]
          (is (= "https://api.honeybadger.io/v1/notices" url)
              "Posts to the HoneyBadger notices endpoint")
          (is (= "test-api-key" (get-in opts [:headers "X-API-Key"]))
              "Sends the API key header"))))))

(deftest ^:multi-threaded notify-is-noop-without-api-key
  (testing "when the api key is not configured"
    (let [calls (atom [])]
      (with-redefs [http/post (fn [url opts] (swap! calls conj {:url url :opts opts}))]
        (honeybadger/notify test-error {:api-key nil})
        (is (empty? @calls)
            "No HTTP calls are made")))))
