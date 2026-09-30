(ns clj-money.images-test
  (:require [clojure.test :refer [deftest is]]
            [integrant.core :as ig]
            [clj-money.images :as images]))

(defn- mem-storage
  []
  (let [store (atom {})
        closed? (atom false)]
    (with-meta
      (reify images/Storage
        (fetch [_ uuid] (get @store uuid))
        (stash [_ uuid content] (swap! store assoc uuid content) uuid)
        (close [_] (reset! closed? true)))
      {:closed? closed?})))

(deftest put-and-get-an-image-using-the-bound-storage
  (let [storage (mem-storage)
        content (.getBytes "image data")]
    (binding [images/*storage* storage]
      (let [uuid (images/put content)]
        (is (string? uuid)
            "The uuid is returned")
        (is (= (seq content)
               (seq (images/get uuid)))
            "The image can be retrieved by uuid")
        (is (= (seq content)
               (seq (images/fetch storage uuid)))
            "The bound storage is used")))))

(deftest reuse-the-default-storage-when-none-is-bound
  (is (identical? (images/storage) (images/storage))
      "The same storage instance is returned on each call"))

(deftest prefer-the-bound-storage
  (let [storage (mem-storage)]
    (binding [images/*storage* storage]
      (is (identical? storage (images/storage))
          "The bound storage is returned"))))

(deftest initialize-and-halt-a-storage-component
  (let [storage (mem-storage)]
    (with-redefs [images/reify-storage (constantly storage)]
      (let [sys (ig/init {::images/storage {::images/strategy ::fake}})]
        (is (identical? storage (::images/storage sys))
            "The storage is created from the config")
        (ig/halt! sys)
        (is @(:closed? (meta storage))
            "The storage is closed when the system is halted")))))
