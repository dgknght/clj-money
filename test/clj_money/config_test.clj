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
                               :sql-app-user "app_user"
                               :sql-app-password "please01"
                               :active-db-strategy :sql})))))

(deftest fetch-a-config-with-unresolvable-references
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"Unresolvable config reference"
                        (config/process {:some-value :config/does-not-exist}))
      "An exception is thrown if the key is absent")
  (is (nil? (:some-value
              (config/process {:some-value :config/deliberately-nil
                               :deliberately-nil nil})))
      "An explicit nil value is allowed"))

(deftest protect-config-file-values-from-environment-variables
  (let [file-config {:env-var-overrides? false
                     :sql-host "localhost"
                     :sql-db-name "money_test"
                     :sql-app-user "app_user"}
        merged (assoc file-config
                      :sql-host "sql"
                      :sql-db-name "money_1_test"
                      :sql-app-user "dev_user"
                      :home "/home/me")
        system-props {:sql-db-name "money_1_test"}]
    (is (= {:env-var-overrides? false
            :sql-host "localhost"
            :sql-db-name "money_1_test"
            :sql-app-user "app_user"
            :home "/home/me"}
           (config/protect-file-config merged file-config system-props))
        "Config file values beat environment variables, but not system properties")
    (is (= merged
           (config/protect-file-config merged
                                       (dissoc file-config :env-var-overrides?)
                                       system-props))
        "Environment variables win when the config file doesn't opt out")))
