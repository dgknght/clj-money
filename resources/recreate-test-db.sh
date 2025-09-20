dropdb money_test --if-exists
createdb money_test --owner ddl_user
lein with-profile test migrate
lein with-profile test partition 2015-01-01 2017-12-31
