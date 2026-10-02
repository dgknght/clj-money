(ns clj-money.test-progress
  "Reports each test as it finishes by appending a character to a file: .
  when the test passed and F when it failed or threw. bin/parallel-test reads
  these files to show progress while its shards run. Loaded by the test
  profile's :injections, but only switched on when the test.progress system
  property names the file."
  (:require [clojure.test :as test]))

(defn progress-hook
  "Returns a hook for clojure.test/report that passes each event on to the
  original report fn and, at the end of each test var, calls write with . or
  F."
  [write]
  (let [failed? (atom false)]
    (fn [report m & args]
      (apply report m args)
      (case (:type m)
        (:fail :error) (reset! failed? true)
        :end-test-var (write (if (first (reset-vals! failed? false)) "F" "."))
        nil))))

(when-let [path (System/getProperty "test.progress")]
  (let [hook (progress-hook #(spit path % :append true))]
    (alter-var-root #'test/report #(partial hook %))))
