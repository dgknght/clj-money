(ns clj-money.cli
  (:require [clojure.tools.cli :refer [parse-opts]]))

(def usage?
  (comp :help? :options))

(defn print-usage
  [{:keys [summary]} {:keys [usage description]}]
  (println description)
  (println "")
  (println "USAGE:")
  (println usage)
  (println "")
  (println "OPTIONS:")
  (println summary))

(defn print-error
  [parsed-opts opts-spec]
  (println "ERROR:")
  (doseq [e (:errors parsed-opts)]
    (println (str "  " e)))
  (println "")
  (print-usage parsed-opts opts-spec))

(defmacro with-options
  [[arg-sym options] & body]
  `(let [opts# ~options
         spec# (:options opts#)
         parsed# (parse-opts (:args opts#)
                             spec#)
         f# (fn* [~arg-sym] ~@body)]
     (cond
       (usage? parsed#)  (print-usage parsed# opts#)
       (:errors parsed#) (print-error parsed# opts#)
       :else             (f# parsed#))))

(def default-options
  [["-h" "--help" "Show usage instructions"
    :id :help?]])
