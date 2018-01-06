alter table transactions_base add column amount numeric(10,2) not null default(0);
alter table transaction_items_base add column negative boolean;
