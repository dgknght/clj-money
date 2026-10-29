(ns clj-money.shell
  "Runs shell commands, like clojure.java.shell, but with a limit on how long
  a command may run"
  (:import [java.io IOException OutputStream]
           java.util.List
           java.util.concurrent.TimeUnit))

(defn sh
  "Runs the command, given as a sequence of its program and arguments, with
  the bytes in :in on its stdin. Returns {:exit :out :err}, like
  clojure.java.shell/sh, or, if the command hasn't finished within
  :timeout-ms, kills it and returns nil. The input is written, and the
  output read, on other threads, so that a full pipe can't block the
  command."
  [args & {:keys [^bytes in timeout-ms]}]
  {:pre [(seq args) timeout-ms]}
  (let [process (.start (ProcessBuilder. ^List (vec args)))
        out (future (slurp (.getInputStream process) :encoding "UTF-8"))
        err (future (slurp (.getErrorStream process) :encoding "UTF-8"))]
    (future
      (try
        (with-open [^OutputStream stdin (.getOutputStream process)]
          (when in
            (.write stdin in)))
        ; the command exited, or was killed, before reading all of it
        (catch IOException _)))
    (if (.waitFor process timeout-ms TimeUnit/MILLISECONDS)
      {:exit (.exitValue process) :out @out :err @err}
      (do (.destroyForcibly process)
          nil))))
