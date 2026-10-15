(ns clj-money.ingestion.reader-test
  (:require [clojure.test :refer [deftest is]]
            [integrant.core :as ig]
            [clj-money.otel :as otel]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.reader]))

(defmethod ing/reader ::stub
  [_config]
  (reify ing/Reader
    (read-receipt [_ _source _entity _opts] {:total 10M})
    (close [_])))

(deftest the-reader-component-is-observable
  (let [recorded (atom [])
        reader (ig/init-key ::ing/reader {::ing/provider ::stub})]
    (with-redefs [otel/record (fn [_instrument value _attributes]
                                (swap! recorded conj value))]
      (is (= {:total 10M}
             (ing/read-receipt reader "source" {:id 1} {}))
          "The result of the underlying reader is returned"))
    (is (= 1 (count @recorded))
        "The duration of the call is recorded")
    (ig/halt-key! ::ing/reader reader)))
