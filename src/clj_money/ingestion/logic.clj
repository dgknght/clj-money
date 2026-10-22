(ns clj-money.ingestion.logic
  "Combines receipt readers, so that a receipt one of them can't read is read
  by another, e.g.:

    {:clj-money.ingestion/provider :clj-money.ingestion.logic/or
     :alternatives [{:clj-money.ingestion/provider :clj-money.ingestion.ocr/ocr}
                    {:clj-money.ingestion/provider :clj-money.ingestion.scale/scale
                     :reader {:clj-money.ingestion/provider :clj-money.ingestion.base64/base64}}]
     :reader {:clj-money.ingestion/provider :clj-money.ingestion.ollama/ollama}}

  Each alternative is a chain of decorators that ends with the :reader, which
  is configured under the :reader of the innermost decorator, and is shared by
  all of them."
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [clj-money.ingestion :as ing]))

(defn- read-bytes
  ^bytes [input]
  (with-open [in (io/input-stream input)]
    (.readAllBytes in)))

(defn- shared
  "Returns the reader with a close that does nothing, so that it's closed
  once, by the or reader, and not by each alternative that ends with it"
  [reader]
  (reify
    ing/ReceiptReader
    (read-receipt [_ source entity opts]
      (ing/read-receipt reader source entity opts))
    ing/Closeable
    (close [_])))

(defn- ending-with
  "Returns the config with the reader under the :reader of its innermost
  decorator. A reader that's already been created is returned as it is."
  [config reader]
  (cond
    (not (map? config)) config
    (:reader config) (update-in config [:reader] ending-with reader)
    :else (assoc config :reader reader)))

(defn- read-first
  "Returns the first receipt read by one of the alternatives. The receipt is
  read into memory first, so that each alternative can read it."
  [alternatives source entity opts]
  (let [content (read-bytes source)]
    (or (some (fn [[i alternative]]
                (when (pos? i)
                  (log/infof "Reading the receipt with alternative %s." (inc i)))
                (ing/read-receipt alternative content entity opts))
              (map-indexed vector alternatives))
        (throw (ex-info "None of the readers could read the receipt." {})))))

(defn- or-reader
  "Returns a receipt reader that reads a receipt with each alternative in
  turn, until one reads it, i.e., returns something other than nil"
  [alternatives reader]
  (reify
    ing/ReceiptReader
    (read-receipt [_ source entity opts]
      (read-first alternatives source entity opts))
    ing/Closeable
    (close [_]
      (doseq [alternative alternatives]
        (ing/close alternative))
      (ing/close reader))))

(defmethod ing/receipt-reader ::or
  [{:keys [alternatives] inner :reader}]
  {:pre [(seq alternatives) inner]}
  (let [reader (ing/receipt-reader inner)]
    (or-reader (mapv #(ing/receipt-reader (ending-with % (shared reader)))
                     alternatives)
               reader)))
