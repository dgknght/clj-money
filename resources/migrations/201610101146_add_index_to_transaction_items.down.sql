drop index uk_transaction_items_index;
alter table transaction_items drop column "index";
create index ix_transaction_items_account on transaction_items (account_id);
