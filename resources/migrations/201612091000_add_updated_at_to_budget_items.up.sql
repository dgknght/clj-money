alter table budget_items
  add column created_at timestamp with time zone not null default now(),
  add column updated_at timestamp with time zone not null default now();
