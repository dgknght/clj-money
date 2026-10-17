(ns clj-money.entities.statement-ingestions-test
  (:require [clojure.test :refer [is]]
            [dgknght.app-lib.test]
            [dgknght.app-lib.test-assertions]
            [clj-money.images.sql]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-entity
                                            find-account
                                            find-statement-ingestion]]
            [clj-money.test-helpers :refer [dbtest]]))

(def ^:private ingestion-context
  (conj basic-context
        #:image{:user "john@doe.com"
                :original-filename "checking.pdf"
                :content "resources/fixtures/statements/checking.pdf"
                :content-type "application/pdf"}
        #:statement-ingestion{:entity "Personal"
                              :image "checking.pdf"
                              :status :pending}))

(dbtest update-a-statement-ingestion
  (with-context ingestion-context
    (let [statement {:account "Checking"
                     :period-start "2026-08-20"
                     :period-end "2026-09-18"
                     :closing-balance 5936.31M
                     :transactions [{:date "2026-09-01"
                                     :amount 2450M
                                     :description "Payroll"}]}
          ingestion (find-statement-ingestion "Personal")
          updated (entities/put
                    (assoc ingestion
                           :statement-ingestion/status :complete
                           :statement-ingestion/account (util/->entity-ref
                                                          (find-account "Checking"))
                           :statement-ingestion/statement statement
                           :statement-ingestion/warnings ["Check the opening balance"]))]
      (is (comparable? #:statement-ingestion{:status :complete
                                             :entity (util/->entity-ref (find-entity "Personal"))
                                             :account (util/->entity-ref (find-account "Checking"))
                                             :statement statement
                                             :warnings ["Check the opening balance"]}
                       (entities/find updated))
          "The statement and warnings keep their shape when stored"))))

(dbtest a-statement-ingestion-requires-a-known-status
  (with-context ingestion-context
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Validation failed"
                          (entities/put (assoc (find-statement-ingestion "Personal")
                                               :statement-ingestion/status :unknown))))))
