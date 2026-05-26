(ns clj-money.images.storage-contract
  (:require [clojure.test :refer [is testing]]
            [clj-money.images :as images]))

(defn run-contract
  "Runs the Storage protocol behavioral assertions against `storage`.
  Call from each implementation's test namespace."
  [storage]
  (let [content (.getBytes "test image content")
        uuid (str (random-uuid))]
    (testing "stash + fetch round trip"
      (images/stash storage uuid content)
      (is (= (seq content)
             (seq (images/fetch storage uuid)))
          "fetched content matches stashed content"))
    (testing "fetch of an unknown uuid"
      (is (nil? (images/fetch storage (str (random-uuid))))
          "returns nil"))))
