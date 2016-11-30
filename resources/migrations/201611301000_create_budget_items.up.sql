create sequence budget_items_id_seq
  increment by 1
  start 1;

create table budget_items (
  id int primary key not null default nextval('budget_items_id_seq'),
  budget_id int not null,
  account_id int not null,
  periods text not null
);

create unique index uk_budget_items_account on budget_items (budget_id, account_id);

alter sequence budget_items_id_seq
  owned by budget_items.id;
