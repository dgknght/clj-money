(ns clj-money.import.gnucash.tasks
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.core.async :as a]
            [clojure.data.xml :as xml]
            [clj-money.import :as import])
  (:import [java.util.zip GZIPInputStream GZIPOutputStream]
           [java.io File FileInputStream FileOutputStream]
           [clojure.data.xml.event StartElementEvent
            CharsEvent
            EndElementEvent]))

(defmacro with-opts-parsing
  [[arg-sym input-opts] & body]
  `(let [opts# ~input-opts
         parsed# (parse-opts (:args opts#)
                             (:opts opts#))
         f# (fn* [~arg-sym]
                 ~@body)]
     (if (-> parsed# :options :help)
       (do
         (println (:title opts#))
         (println "")
         (println (:description opts#))
         (println "")
         (println "Usage:")
         (doseq [use# (:usage opts#)]
           (println " " use#))
         (println "")
         (println "Options:")
         (println (:summary parsed#)))
       (f# parsed#))))

(defn- process-output
  "Writes the records to the next output file and resets the record buffer"
  [{:keys [records
           output-folder
           file-name
           output-paths]
    :as context}]
  (if (seq records)
    (let [output-path (format "%s/%s_%s.edn.gz"
                              output-folder
                              file-name
                              (count output-paths))]
      (println "compressing and writing records...")
      (binding [*print-meta* true]
        (with-open [writer (io/writer (GZIPOutputStream. (FileOutputStream. output-path)))]
          (with-precision 4
            (.write writer (prn-str records)))))
      (println output-path)
      (-> context
          (assoc :records [])
          (update-in [:output-paths] #(conj % output-path))))
    context))

(def ^:private chunk-file-options
  [["-h" "--help" "Show this help message"]
   ["-m" "--maximum MAXIMUM" "The maximum number of records to include in each output file"
    :parse-fn parse-long
    :default 10000]])

(defn- parse-chunk-file-opts
  "Accepts the raw arguments passed into chunk-file
  and returns a map containing the information needed
  to service the request"
  [args]
  (let [opts (parse-opts args chunk-file-options)]
    (when (-> opts :options :help)
      (println (:summary opts))
      (System/exit 0))
    (let [input-path (-> opts :arguments first)
          input-file (File. input-path)
          output-folder (.getParent input-file)
          file-name (.getName input-file)]
      {:options (:options opts)
       :input-file input-file
       :file-name file-name
       :output-folder output-folder})))

(defn- evaluate-output
  "Evaluates the output for the chunking process
  and writes the file and resets the record buffer if the
  maximum number of records has been reached"
  ([context _] (evaluate-output context))
  ([{:keys [records max-record-count] :as context}]
   (if (>= (count records) max-record-count)
     (process-output context)
     context)))

(defn- chunk-file-state
  [args]
  (let [{:keys [input-file
                output-folder
                file-name]
         {:keys [maximum]} :options} (parse-chunk-file-opts args)]
    {:output-paths []
     :input-file input-file
     :records []
     :max-record-count maximum
     :file-name (second (re-matches #"^(.+)(\..+)$" file-name))
     :output-folder output-folder}))

(defn chunk-file
  "Accepts a path to a gnucash file and creates multiple, smaller files
  that container the same data, returning the paths to the new files"
  [& args]
  (let [records-chan (a/chan)
        state (chunk-file-state args)
        result (a/reduce #(-> %1
                              (update-in [:records] conj %2)
                              evaluate-output)
                         state
                         records-chan)]
    (println "reading the source file...")
    (with-open [input-stream (FileInputStream. (:input-file state))]
      (import/read-source :gnucash [input-stream] records-chan))
    (process-output (a/<!! result))
    (println "\nDone")))

(def ^:private account-history-options
  [["-h" "--help" "Show this help message"]
   ["-f" "--file FILE" "The gnucash file to be read"
    :missing "You must specify a gnucash file"]
   ["-a" "--account ACCOUNT" "The name of the account for which history is to be extracted"]])

(def ^:private account-history-desc
  {:title "Account History"
   :description "Extract the history of an account from a GnuCash file."
   :usage ["lein account-history -f <path-to-gnuash-file>"]})

(defn- gzip-input-stream
  [input]
  (GZIPInputStream. input))

(defmulti handle-account-history-event
  (fn [_ctx event]
    (type event)))

(defmethod handle-account-history-event StartElementEvent
  [ctx event]
  (update-in ctx [:elem-stack] conj (name (:tag event))))

(defmethod handle-account-history-event EndElementEvent
  [ctx _event]
  (pprint {:end-elem ctx})
  (-> ctx
      (update-in [:elem-stack] pop)
      (assoc :content [])))

(defmethod handle-account-history-event CharsEvent
  [ctx event]
  (update-in ctx [:content] conj (:str event)))

(defn account-history
  [& args]
  (with-opts-parsing [opts (assoc account-history-desc
                                  :args args
                                  :opts account-history-options)]
    (let [events (-> opts
                     :options
                     :file
                     io/input-stream
                     gzip-input-stream
                     (xml/event-seq {}))]
      (reduce handle-account-history-event
              {:elem-stack `()
               :content []}
              (take 20 events)))))
