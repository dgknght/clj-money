create sequence lots_id_seq
  increment by 1
  start 1;

create table lots (
  id int primary key not null default nextval('lots_id_seq'),
  commodity_id int not null,
  account_id int not null,
  purchase_date bigint not null,
  shares_purchased decimal(10,4) not null,
  shares_owned decimal(10,4) not null default(0),
  created_at timestamp with time zone not null default now(),
  updated_at timestamp with time zone not null default now()
);

create index ix_lots_commodity_id on lots (commodity_id);
create index ix_lots_account_id on lots (account_id);

alter sequence lots_id_seq
  owned by lots.id;
