(ns clj-money.ingestion.mock
  (:require [clj-money.ingestion :as ing]))

(defmethod ing/reader ::mock
  [_config]
  (reify ing/Reader
    (read-receipt [_ _source _entity _opts])
    (close [_])))
