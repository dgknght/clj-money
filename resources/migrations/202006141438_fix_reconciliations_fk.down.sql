alter table transaction_items
  drop constraint if exists fk_transaction_items_account_id;

alter table transaction_items
  add constraint transaction_items_base_account_id_fkey foreign key (account_id) references accounts (id) on delete cascade;
