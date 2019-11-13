alter table transaction_items_base
  alter column quantity type numeric(10, 2),
  alter column balance  type numeric(10, 2),
  alter column value    type numeric(10, 2);

alter table transactions_base
  alter column value type numeric(10, 2);

alter table accounts
  alter column quantity type numeric(10, 2),
  alter column value    type numeric(10, 2);

alter table prices_base
  alter column price type numeric(10, 2);
