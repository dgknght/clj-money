(ns clj-money.crypto-test
  (:require [clojure.test :refer [deftest is]]
            [clj-money.crypto :as crypto]))

(def ^:private test-key "MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY=")
(def ^:private other-key "ZmVkY2JhOTg3NjU0MzIxMGZlZGNiYTk4NzY1NDMyMTA=")

(deftest encrypt-and-decrypt-a-secret
  (let [encrypted (crypto/encrypt "s3cret p@ssword" test-key)]
    (is (not= "s3cret p@ssword" encrypted)
        "The encrypted value doesn't contain the secret")
    (is (= "s3cret p@ssword" (crypto/decrypt encrypted test-key))
        "The secret is decrypted with the same key")
    (is (not= encrypted (crypto/encrypt "s3cret p@ssword" test-key))
        "Each encryption uses a new IV")))

(deftest decrypting-with-another-key-fails
  (is (thrown? javax.crypto.AEADBadTagException
               (crypto/decrypt (crypto/encrypt "secret" test-key) other-key))))

(deftest a-key-is-required
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"No encryption key"
                        (crypto/encrypt "secret" nil))))

(deftest the-key-must-be-32-bytes
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"must be 32 bytes"
                        (crypto/encrypt "secret" "c2hvcnQ="))))
