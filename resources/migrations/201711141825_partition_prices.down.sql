drop table prices_base;
alter table prices
  add constraint fk_prices_commodity
      foreign key (commodity_id)
      references commodities (id)
      on delete cascade;
