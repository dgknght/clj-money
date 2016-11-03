alter table transaction_items add column "index" int not null;
drop index ix_transaction_items_account;
create unique index uk_transaction_items_index on transaction_items (account_id, "index");
