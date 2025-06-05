(ns clj-money.config-test
  (:require [clojure.test :refer [deftest is]]
            [clj-money.config :as config]))

(deftest fetch-a-config-with-resolved-references
  (is (= {:strategies {:sql {:clj-money.db/strategy :clj-money.db/sql
                             :host "localhost"
                             :post 5432
                             :user "app_user"
                             :password "please01"
                             :dbtype "postgresql"
                             :dbname "money_test"}}
          :active :sql}
         (:db (config/process {:db {:strategies {:sql {:clj-money.db/strategy :clj-money.db/sql
                                                       :host "localhost"
                                                       :post 5432
                                                       :user :config/sql-app-user
                                                       :password :config/sql-app-password
                                                       :dbtype "postgresql"
                                                       :dbname "money_test"}}
                                    :active :config/active-db-strategy}
                               :config/sql-app-user "app_user"
                               :config/sql-app-password "please01"
                               :config/active-db-strategy :sql})))))

(deftest fetch-a-config-with-unresolvable-references
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"Unresolvable config reference"
                        (config/process {:some-value :config/does-not-exist}))))
