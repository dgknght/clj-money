dropdb money_development
createdb money_development
lein migrate
lein partition 2008-01-01 2021-12-31
