(ns clj-money.shell-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clj-money.shell :as shell])
  (:import java.io.File
           java.lang.ProcessHandle
           java.util.concurrent.TimeUnit))

(deftest the-output-of-a-command-is-returned
  (is (= {:exit 0 :out "hello\n" :err ""}
         (shell/sh ["echo" "hello"] :timeout-ms 5000))))

(deftest the-input-is-given-to-the-command-on-stdin
  (is (= "hello"
         (:out (shell/sh ["cat"]
                         :in (.getBytes "hello" "UTF-8")
                         :timeout-ms 5000)))))

(deftest input-larger-than-a-pipe-can-hold-is-given-to-the-command
  (let [input (byte-array (* 1024 1024) (byte 65))]
    (is (= "1048576"
           (str/trim (:out (shell/sh ["wc" "-c"]
                                     :in input
                                     :timeout-ms 5000)))))))

(deftest the-exit-code-and-errors-of-a-failed-command-are-returned
  (is (= {:exit 3 :out "" :err "oops\n"}
         (shell/sh ["sh" "-c" "echo oops >&2; exit 3"] :timeout-ms 5000))))

(deftest a-command-that-doesnt-finish-in-time-is-killed
  (let [pid-file (doto (File/createTempFile "shell" ".pid")
                   (.deleteOnExit))
        started (System/currentTimeMillis)]
    (is (nil? (shell/sh ["sh" "-c" (format "echo $$ > %s; exec sleep 30"
                                           pid-file)]
                        :timeout-ms 500))
        "Nothing is returned")
    (is (> 10000 (- (System/currentTimeMillis) started))
        "The command isn't waited for")
    (let [pid (parse-long (str/trim (slurp pid-file)))]
      (is (not (some-> (ProcessHandle/of pid)
                       (.orElse nil)
                       (.onExit)
                       (.get 5 TimeUnit/SECONDS)
                       (.isAlive)))
          "The command is killed"))))
