dropdb money_test
createdb money_test
lein with-profile test migrate
lein with-profile test partition 2015-01-01 2017-12-31
