(ns clj-money.api.test-helper
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [environ.core :refer [env]]
            [clj-money.test-helpers :refer [with-authentication]]))

(defmacro deftest-list
  [name {:keys [context
                storage
                resource-name
                find-user-fn
                find-other-user-fn
                list-fn
                params-fn
                expectation-fn]
         :or {resource-name "resource"
              context 'context
              storage (env :db)
              find-user-fn 'find-user
              find-other-user-fn 'find-other-user}}]
  `(deftest ~name
     (let [context# (serialization/realize ~storage ~context)
           params# {:params (~params-fn context#)}]
       (testing (format "A user can get a %s list from his own entity" ~resource-name)
         (let [response# (with-authentication (~find-user-fn context#)
                           (~list-fn params#))]
           (is (= 200 (:status response#)) "The response is successful")
           (is (~expectation-fn (:body response#))
               "The response contains expected resources.")))
       (testing (format "A user cannot get a %s list from someone else's entities" ~resource-name)
         (let [error# (try (with-authentication (~find-other-user-fn context#)
                             (~list-fn params#))
                           (catch Exception e#
                             e#))]
           (is (= clojure.lang.ExceptionInfo (type error#))
               "An exception is thrown"))))))

(defmacro deftest-get-one
  [name {:keys [context
                storage
                resource-name
                find-user-fn
                find-other-user-fn
                get-one-fn
                params-fn
                expectation-fn]
         :or {resource-name "resource"
              context 'context
              storage (env :db)
              find-user-fn 'find-user
              find-other-user-fn 'find-other-user}}]
  `(deftest ~name
     (let [context# (serialization/realize ~storage ~context)
           params# {:params (~params-fn context#)}]
       (testing (format "A user can get a %s from his own entity" ~resource-name)
         (let [response# (with-authentication (~find-user-fn context#)
                           (~get-one-fn params#))]
           (is (= 200 (:status response#)) "The response is successful")
           (is (~expectation-fn (:body response#))
               "The response contains expected resource.")))
       (testing (format "A user cannot get a %s from someone else's entities" ~resource-name)
         (let [error# (try (with-authentication (~find-other-user-fn context#)
                             (~get-one-fn params#))
                           (catch Exception e#
                             e#))]
           (is (= clojure.lang.ExceptionInfo (type error#))
               "An exception is thrown"))))))


(defmacro deftest-create
  [name {:keys [context
                storage
                resource-name
                find-user-fn
                find-other-user-fn
                create-fn
                create-params-fn
                select-resources-fn
                compare-fn
                skip-auth-failure-test]
         :or {resource-name "resource"
              context 'context
              storage (env :db)
              find-user-fn 'find-user
              find-other-user-fn 'find-other-user
              select-resources-fn 'select-resources}}]
  `(deftest ~name
     (let [context# (serialization/realize ~storage ~context)
           params# {:params (~create-params-fn context#)}]
       (when-not ~skip-auth-failure-test
         (testing (format "A user cannot create a %s for someone else's entities" ~resource-name)
           (let [error# (try (with-authentication (~find-other-user-fn context#)
                               (~create-fn params#))
                             (catch Exception e#
                               e#)) 
                 resources# (~select-resources-fn context#)]
             (is (= clojure.lang.ExceptionInfo (type error#)) "An exception is thrown")
             (is (not-any? ~compare-fn resources#)
                 (format "The %s is not created." ~resource-name)))))
       (testing (format "A user can create a %s for his own entity" ~resource-name)
         (let [response# (with-authentication (~find-user-fn context#)
                           (~create-fn params#))
               resources# (~select-resources-fn context#)]
           (if-not (= 201 (:status response#))
             (pprint {:invalid-resource (:body response#)}))
           (is (= 201 (:status response#)) "The response is a successful creation")
           (is (some ~compare-fn resources#)
               (format "The response contains the new %s" ~resource-name)))))))

(defmacro deftest-update
  [name {:keys [context
                resource-name
                find-resource-fn
                find-updated-resource-fn
                find-user-fn
                find-other-user-fn
                update-fn
                update-params
                prepare-update-fn
                comparison-fn
                storage]
         :or {resource-name "resource"
              context 'context
              storage (env :db)
              find-user-fn 'find-user
              find-other-user-fn 'find-other-user
              find-resource-fn 'find-resource }}]
  `(deftest ~name
     (let [context# (serialization/realize ~storage ~context)
          resource# (~find-resource-fn context#)
          to-save# {:params (if ~update-params
                              (assoc ~update-params
                                     :id
                                     (:id resource#))
                              (~prepare-update-fn resource#))}]
      (testing (format "A user cannot update a %s for another user's entity" ~resource-name)
        (let [error# (try (with-authentication (~find-other-user-fn context#)
                       (~update-fn to-save#))
                          (catch Exception e#
                            e#))]
          (is (= clojure.lang.ExceptionInfo (type error#)))))
      (testing (format "A user can update a %s in their entity" ~resource-name)
        (let [response# (with-authentication (~find-user-fn context#)
                         (api/update to-save#))
              updated# (~find-updated-resource-fn resource#)]
          (is (= 200 (:status response#)) "The response is successful")
          (is (~comparison-fn updated#)
              (format "The %s is updated in the data store." ~resource-name)))))))

(defmacro deftest-delete
  [name {:keys [context
                find-resource-fn
                find-user-fn
                find-other-user-fn
                select-resources-fn
                delete-fn
                resource-name
                storage
                delete-keys]
         :as test-def
         :or {resource-name "resource"
              context 'context
              storage (env :db)
              find-user-fn 'find-user
              find-other-user-fn 'find-other-user
              find-resource-fn 'find-resource
              select-resources-fn 'select-resources
              delete-keys [:id]}}]
  `(deftest ~name
     (let [context# (serialization/realize ~storage ~context)
           resource# (~find-resource-fn context#)
           params# (select-keys resource# ~delete-keys)]
       (testing (format "A user cannot delete a %s from someone else's entity" ~resource-name)
         (let [error# (try
                        (with-authentication (~find-other-user-fn context#)
                          (~delete-fn {:params params#}))
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
                           (~delete-fn {:params params#}))
               resource-ids# (->> (~select-resources-fn context#)
                                  (map :id)
                                  (into #{}))]
           (is (= 204 (:status response#))
               "The response status is successful without content.")
           (is (not (resource-ids# (:id resource#)))
               "The resource is no longer available after delete."))))))
