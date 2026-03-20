#!/bin/bash

dropdb --if-exists money_development && \
  lein do create-sql, migrate, migrate-auxiliary, partition 2008-01-01 2026-12-31

dropdb --if-exists money_test && \
  lein with-profile test do create-sql, migrate, migrate-auxiliary, partition 2015-01-01 2017-12-31
