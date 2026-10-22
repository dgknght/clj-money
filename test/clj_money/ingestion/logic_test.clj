(ns clj-money.ingestion.logic-test
  (:require [clojure.test :refer [deftest is]]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.base64 :as b64]
            [clj-money.ingestion.logic :as logic]
            [clj-money.ingestion.scale :as scale]
            [clj-money.ingestion.helpers :as hlp])
  (:import java.io.ByteArrayInputStream))

(def ^:private receipt
  (.getBytes "receipt" "US-ASCII"))

(defn- read-or
  "Reads the receipt from a stream, so that it can only be read once, with
  an or reader whose alternatives are the first reader and the scale and
  base64 decorators of the final reader. Returns the result and the calls
  to each reader."
  [first-result final-result]
  (let [first-reader (hlp/inspectable-reader :result first-result)
        final-reader (hlp/inspectable-reader :result final-result)
        reader (ing/receipt-reader {::ing/provider ::logic/or
                                    :alternatives [first-reader
                                                   {::ing/provider ::scale/scale
                                                    :reader {::ing/provider ::b64/base64}}]
                                    :reader final-reader})
        result (try
                 (ing/read-receipt reader
                                   (ByteArrayInputStream. receipt)
                                   {}
                                   {:model "m"})
                 (finally
                   (ing/close reader)))]
    {:result result
     :first (hlp/calls first-reader)
     :final (hlp/calls final-reader)}))

(deftest the-first-receipt-read-is-returned
  (let [{:keys [result first final]} (read-or {:total 1M} {:total 2M})]
    (is (= {:total 1M} result)
        "The result of the first alternative is returned")
    (is (= (seq receipt)
           (seq (get-in first [:read-receipt 0 0])))
        "The first alternative is given the receipt")
    (is (empty? (:read-receipt final))
        "The other alternatives aren't used")))

(deftest a-receipt-not-read-is-read-by-the-next-alternative
  (let [{:keys [result final]
         {[[contents _ opts]] :read-receipt} :final}
        (read-or nil {:total 2M})]
    (is (= {:total 2M} result)
        "The result of the next alternative is returned")
    (is (= {:images [(b64/encode receipt)]} contents)
        "The alternative ends with the shared reader")
    (is (= "m" (:model opts))
        "The options are passed to the alternative")
    (is (= 1 (:close final))
        "The shared reader is closed once")))

(deftest a-receipt-no-alternative-reads-cant-be-read
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"None of the readers could read the receipt"
                        (read-or nil nil))))

(deftest the-alternatives-are-closed
  (is (= 1 (get-in (read-or {:total 1M} nil) [:first :close]))))
