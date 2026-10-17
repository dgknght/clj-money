(ns clj-money.ingestion.reader-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [integrant.core :as ig]
            [clj-money.otel :as otel]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.reader]
            [clj-money.ingestion.pdf :as pdf]
            [clj-money.otel.ingestion :as otel-ing]))

(def ^:private closed (atom []))

(defn- stub-reader
  [name]
  (reify
    ing/ReceiptReader
    (read-receipt [_ _source _entity _opts] {:total 10M :reader name})
    ing/StatementReader
    (read-statement [_ _source _entity _opts] {:reader name})
    ing/Closeable
    (close [_] (swap! closed conj name))))

(defmethod ing/receipt-reader ::stub
  [{:keys [name] :or {name "unnamed"}}]
  (stub-reader name))

(defmethod ing/statement-reader ::stub
  [{:keys [name] :or {name "unnamed"}}]
  (stub-reader name))

(deftest each-kind-of-document-can-have-its-own-reader
  (reset! closed [])
  (let [config {::ing/provider ::stub
                :name "default"
                :statements {::ing/provider ::stub
                             :name "local"}}
        receipts (ig/init-key ::ing/receipt-reader config)
        statements (ig/init-key ::ing/statement-reader config)]
    (is (= "default" (:reader (ing/read-receipt receipts "source" {:id 1} {})))
        "A kind of document without its own reader uses the default")
    (is (= "local" (:reader (ing/read-statement statements "source" {:id 1} {})))
        "A kind of document with its own reader uses it")
    (ig/halt-key! ::ing/receipt-reader receipts)
    (ig/halt-key! ::ing/statement-reader statements)
    (is (= #{"default" "local"} (set @closed))
        "Each reader is closed")))

(deftest a-document-reader-takes-nothing-from-the-default
  (let [reader (ig/init-key ::ing/receipt-reader {::ing/provider ::stub
                                                  :name "default"
                                                  :receipts {::ing/provider ::stub}})]
    (is (= "unnamed" (:reader (ing/read-receipt reader "source" {:id 1} {})))
        "The top-level name isn't given to the receipt reader")))

(defmethod ing/statement-reader ::recording
  [{:keys [read]}]
  (reify
    ing/StatementReader
    (read-statement [_ source _entity _opts]
      (reset! read source)
      {})
    ing/Closeable
    (close [_] (swap! closed conj "recording"))))

(defmethod ing/receipt-reader ::receipts-only
  [_]
  (stub-reader "receipts only"))

(deftest a-provider-that-cant-read-statements-is-explained
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"can't read statements"
                        (ig/init-key ::ing/statement-reader
                                     {::ing/provider ::receipts-only}))))

(deftest readers-are-composed-in-the-config
  (reset! closed [])
  (let [recorded (atom [])
        read (atom nil)
        reader (ig/init-key ::ing/statement-reader
                            {:statements
                             {::ing/provider ::otel-ing/observable
                              :reader {::ing/provider ::pdf/pdf
                                       :reader {::ing/provider ::recording
                                                :read read}}}})]
    (with-redefs [otel/record (fn [_instrument value attributes]
                                (swap! recorded conj [value attributes]))]
      (ing/read-statement reader
                          (io/file "fixtures/statements/checking.pdf")
                          {:id 1}
                          {}))
    (is (re-find #"Prairie State Bank" (:text @read))
        "The innermost reader is given what the decorators make of the source")
    (is (= "recording"
           (get-in @recorded [0 1 "gen_ai.provider.name"]))
        "The calls are measured as calls to the innermost reader")
    (ig/halt-key! ::ing/statement-reader reader)
    (is (= ["recording"] @closed)
        "Closing the reader closes the reader it decorates")))

(deftest a-decorator-of-statements-cant-read-receipts
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"can't read receipts"
                        (ig/init-key ::ing/receipt-reader
                                     {::ing/provider ::pdf/pdf
                                      :reader {::ing/provider ::stub}}))))
