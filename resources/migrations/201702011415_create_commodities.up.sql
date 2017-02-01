create sequence commodities_id_seq
  increment by 1
  start 1;

create table commodities (
  id int primary key not null default nextval('commodities_id_seq'),
  entity_id int not null,
  name varchar(100) not null,
  symbol varchar(10) not null,
  exchange varchar(10) not null,
  created_at timestamp with time zone not null default now(),
  updated_at timestamp with time zone not null default now()
);

create unique index uk_commodities_symbol on commodities (entity_id, exchange, symbol);
create unique index uk_commodities_name on commodities (entity_id, exchange, name);

alter sequence commodities_id_seq
  owned by commodities.id;
