alter table accounts
  add constraint fk_accounts_entity foreign key (entity_id) references entities (id) on delete cascade,
  add constraint fk_accounts_account foreign key (parent_id) references accounts (id) on delete cascade;
alter table attachments
  add constraint fk_attachments_transaction foreign key (transaction_id) references transactions (id) on delete cascade,
  add constraint fk_attachments_image foreign key (image_id) references images (id) on delete cascade;
alter table budget_items
  add constraint fk_budget_items_budget foreign key (budget_id) references budgets (id) on delete cascade,
  add constraint fk_budget_items_account foreign key (account_id) references accounts (id) on delete cascade;
alter table budgets
  add constraint fk_budgets_entity foreign key (entity_id) references entities (id) on delete cascade;
alter table commodities
  add constraint fk_commodities_entity foreign key (entity_id) references entities (id) on delete cascade;
alter table entities
  add constraint fk_entities_user foreign key (user_id) references users (id) on delete cascade;
alter table images
  add constraint fk_images_user foreign key (user_id) references users (id) on delete cascade;
alter table imports
  add constraint fk_imports_image foreign key (image_id) references images (id) on delete cascade;
alter table lots_transactions
  add constraint fk_lots_transactions_transaction foreign key (transaction_id) references transactions (id) on delete cascade,
  add constraint fk_lots_transactions_lot foreign key (lot_id) references lots (id) on delete cascade;
alter table lots
  add constraint fk_lots_commodity foreign key (commodity_id) references commodities (id) on delete cascade,
  add constraint fk_lots_account foreign key (account_id) references accounts (id) on delete cascade;
alter table prices
  add constraint fk_prices_commodity foreign key (commodity_id) references commodities (id) on delete cascade;
alter table reconciliations
  add constraint fk_reconciliations_account foreign key (account_id) references accounts (id) on delete cascade;
alter table transactions
  add constraint fk_transactions_entity foreign key (entity_id) references entities (id) on delete cascade;
alter table transaction_items
  add constraint fk_transaction_items_transaction foreign key (transaction_id) references transactions (id) on delete cascade,
  add constraint fk_transaction_items_account foreign key (account_id) references accounts (id) on delete cascade,
  add constraint fk_transaction_items_reconciliation foreign key (reconciliation_id) references reconciliations (id) on delete cascade;
