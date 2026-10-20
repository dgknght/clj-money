(ns clj-money.ingestion.helpers
  (:require [clj-money.ingestion :as ing]))

(defprotocol Inspectable
  (calls [_] "Retrieve the calls to the reader"))

(defn inspectable-reader
  [& {:keys [result] :or {result :result}}]
  (let [calls (atom {:read-receipt []
                     :read-statement []
                     :close 0})]
    (reify
      ing/ReceiptReader
      (read-receipt [_ source entity opts]
        (swap! calls update-in [:read-receipt] conj [source entity opts])
        result)

      ing/StatementReader
      (read-statement [_ source entity opts]
        (swap! calls update-in [:read-statement] conj [source entity opts])
        result)

      ing/Closeable
      (close [_] (swap! calls update-in [:close] inc))

      Inspectable
      (calls [_] @calls))))

(defn- read-through
  [create-reader read-fn source {:keys [provider] :as opts}]
  (let [inner (inspectable-reader :result ::result)
        reader (create-reader (merge {::ing/provider provider
                                      :reader inner}
                                     opts))
        result (read-fn reader source {} opts)]
    (ing/close reader)
    {:result result
     :calls (calls inner)}))

(defn read-receipt
  "Reads the receipt with the :provider reader, configured with the options
  and decorating an inspectable reader, then closes it. The options are
  also passed to the read. Returns the :result and the :calls to the
  inspectable reader."
  [receipt & {:as opts}]
  (read-through ing/receipt-reader ing/read-receipt receipt opts))

(defn read-statement
  "Reads the statement as read-receipt reads a receipt"
  [statement & {:as opts}]
  (read-through ing/statement-reader ing/read-statement statement opts))
