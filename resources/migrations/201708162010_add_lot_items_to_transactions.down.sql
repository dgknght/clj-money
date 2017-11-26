alter table transactions
  drop column lot_items,
  add column lot_id int;
