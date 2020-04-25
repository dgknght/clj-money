alter table budget_items
  drop column periods,
  add column periods numeric(12,4)[] not null;
