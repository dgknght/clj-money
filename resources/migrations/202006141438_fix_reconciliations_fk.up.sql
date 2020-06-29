alter table transaction_items
  drop constraint if exists transaction_items_base_account_id_fkey;

alter table transaction_items
  add constraint fk_transaction_items_account_id foreign key (account_id) references accounts (id) on delete no action;
