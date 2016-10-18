drop index ix_transaction_items_index;
create unique index uk_transaction_items_index on transaction_items (account_id, "index");
