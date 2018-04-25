alter table accounts rename balance to quantity;
alter table accounts add column value numeric(10, 2);
alter table transaction_items_base rename amount to quantity;
