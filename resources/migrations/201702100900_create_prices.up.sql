create sequence prices_id_seq
  increment by 1
  start 1;

create table prices (
  id int primary key not null default nextval('prices_id_seq'),
  commodity_id int not null,
  trade_date bigint not null,
  price decimal(10,2) not null,
  created_at timestamp with time zone not null default now(),
  updated_at timestamp with time zone not null default now()
);

create unique index uk_prices_trade_date on prices (commodity_id, trade_date);

alter sequence prices_id_seq
  owned by prices.id;
