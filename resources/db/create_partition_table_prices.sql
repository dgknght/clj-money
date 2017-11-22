create table if not exists prices{{table-suffix}} (
  check (
    trade_date >= '{{start-of-period}}' and
    trade_date < '{{start-of-next-period}}')
) inherits (prices_base);
alter table prices{{table-suffix}}
  add constraint fk_prices{{table-suffix}}_commodities foreign key (commodity_id) references commodities (id) on delete cascade;
