alter table transactions
  add column lot_items text,
  drop column lot_id;
