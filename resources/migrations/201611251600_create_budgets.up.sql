create sequence budgets_id_seq
  increment by 1
  start 1;

create table budgets (
  id int primary key not null default nextval('budgets_id_seq'),
  entity_id int not null,
  name varchar(50) not null,
  start_date bigint not null,
  period varchar(20) not null,
  period_count smallint not null
);

create unique index uk_budgets_name on budgets (entity_id, name);

alter sequence budgets_id_seq
  owned by budgets.id;
