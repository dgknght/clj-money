(ns clj-money.test-runner
  (:require [cljs.test :as t]
            [clj-money.dates-test]
            [clj-money.account-history-test]
            [clj-money.accounts-test]
            [clj-money.budgets-test]
            [clj-money.util-test]
            [clj-money.transactions-test]
            [clj-money.scheduled-transactions-test]
            [clj-money.routes-test]
            [clj-money.views.recent-transactions-test]
            [clj-money.components-test]
            [clj-money.views.receipts-test]
            [clj-money.views.transactions.form-test]
            [clj-money.views.transactions.page-test]
            [clj-money.views.trades.form-test]
            [clj-money.views.accounts.filter-test]
            [clj-money.views.accounts.page-test]
            [clj-money.views.accounts.bulk-edit-test]
            [clj-money.views.reconciliations-test]
            [clj-money.views.statement-upload-test]
            [clj-money.views.budgets-test]
            [clj-money.views.attachments-test]
            [clj-money.reconciliations-test]
            [clj-money.api.trading-test]
            [clj-money.api.transfers-test]
            [clj-money.api.cli-logins-test]
            [clj-money.return-path-test]
            [clj-money.commands-test]
            [clj-money.nav-test]))

(defmethod t/report [:cljs.test/default :end-run-tests] [m]
  (set! (.-exitCode js/process) (if (t/successful? m) 0 1)))

(defn -main [& _args]
  (t/run-tests
    'clj-money.routes-test
    'clj-money.dates-test
    'clj-money.account-history-test
    'clj-money.accounts-test
    'clj-money.budgets-test
    'clj-money.util-test
    'clj-money.transactions-test
    'clj-money.scheduled-transactions-test
    'clj-money.views.recent-transactions-test
    'clj-money.components-test
    'clj-money.views.receipts-test
    'clj-money.views.transactions.form-test
    'clj-money.views.transactions.page-test
    'clj-money.views.trades.form-test
    'clj-money.views.accounts.filter-test
    'clj-money.views.accounts.page-test
    'clj-money.views.accounts.bulk-edit-test
    'clj-money.views.reconciliations-test
    'clj-money.views.statement-upload-test
    'clj-money.views.budgets-test
    'clj-money.views.attachments-test
    'clj-money.reconciliations-test
    'clj-money.api.trading-test
    'clj-money.api.transfers-test
    'clj-money.api.cli-logins-test
    'clj-money.return-path-test
    'clj-money.commands-test
    'clj-money.nav-test))
