alter table transaction_items_base
  alter column quantity type numeric(12, 4),
  alter column balance  type numeric(12, 4),
  alter column value    type numeric(12, 4);

alter table transactions_base
  alter column value type numeric(12, 4);

alter table accounts
  alter column quantity type numeric(12, 4),
  alter column value    type numeric(12, 4);

alter table prices_base
  alter column price type numeric(12, 4);
