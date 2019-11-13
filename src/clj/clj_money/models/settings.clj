(ns clj-money.models.settings
  (:refer-clojure :exclude [get])
  (:require [clj-money.models.helpers :refer [with-storage]]
            [clj-money.models.storage :refer [put-setting
                                              get-setting]]))

(defn put
  [storage-spec setting-name setting-value]
  (with-storage [s storage-spec]
    (put-setting s (name setting-name) (prn-str setting-value))))

(defn get
  [storage-spec setting-name]
  (with-storage [s storage-spec]
    (when-let [value (get-setting s (name setting-name))]
      (read-string value))))
