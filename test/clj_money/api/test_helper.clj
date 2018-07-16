(ns clj-money.api.test-helper
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clj-money.test-helpers :refer [with-authentication]]))

(defmacro deftest-delete
  [name context {:keys [find-resource-fn
                        find-user-fn
                        find-other-user-fn
                        select-resources-fn
                        delete-fn
                        resource-name
                        storage]
                 :as test-def
                 :or {resource-name "resource"}}]
  `(deftest ~name
     (let [context# (serialization/realize ~storage ~context)
           resource# (~find-resource-fn context#)]
       (testing (format "A user cannot delete a %s from someone else's entity" ~resource-name)
         (let [error# (try
                        (with-authentication (~find-other-user-fn context#)
                          (~delete-fn {:params {:id (:id resource#)}}))
                        (catch java.lang.Exception e#
                          e#))
               resource-ids# (->> (~select-resources-fn context#)
                                  (map :id)
                                  (into #{}))]
           (is (= clojure.lang.ExceptionInfo (type error#)) "An error is thrown")
           (is (resource-ids# (:id resource#))
               "The resource is still available after the attempt to delete.")))
       (testing (format "A user can delete a %s from his own entity" ~resource-name)
           (let [response# (with-authentication (~find-user-fn context#)
                           (~delete-fn {:params {:id (:id resource#)}}))
               resource-ids# (->> (~select-resources-fn context#)
                                  (map :id)
                                  (into #{}))]
           (is (= 204 (:status response#))
               "The response status is successful without content.")
           (is (not (resource-ids# (:id resource#)))
               "The resource is no longer available after delete."))))))
