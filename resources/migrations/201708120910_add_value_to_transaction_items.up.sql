alter table transaction_items add column value numeric (10, 2);
update transaction_items set
  value = amount;
alter table transaction_items alter column value set not null;
