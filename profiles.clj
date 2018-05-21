{:dev
 {:env
  {:db "postgresql://app_user:please01@localhost/money_development"
   :partition-period "year"}
  :show-error-messages? true
  }
 :test
 {:env
  {:db "postgresql://app_user:please01@localhost/money_test" 
   :partition-period "month"
   :mailer-host "testmailer.com"
   :mailer-from "no-reply@clj-money.com"
   :application-name "clj-money"
   :show-error-messages? true}}}
