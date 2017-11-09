{:dev
 {:env
  {:db "postgresql://app_user:please01@localhost/money_development"}}
 :test
 {:env
  {:db "postgresql://app_user:please01@localhost/money_test" 
   :mailer-host "testmailer.com"
   :mailer-from "no-reply@clj-money.com"
   :application-name "clj-money"}}}
