# Unpartition Transactions

Currently the `transaction` table and all of its children are partitioned
by date. This was to support an open-ended number of users. Now the plan
is to host this privately and have invitation-only users, so the number
of transactions should be significantly lower. This means we can simplify
a lot of logic on the transaction-related code.

- Change the database structure (`resources/migrations/202005081349_create_initial_schema.up.sql`)
  so that all tables that are currently partitioned are not, which the
  exception of `price` and `cached_price`, which will continue to be
  partitioned. For targeted tables:
  - Use `integer` for the data type of the `id` column.
  - Remove the `transaction_date` column that was only there to support
    partitioning.
  - Create a sequence to generate the values for the `id` column.
