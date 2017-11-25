create table if not exists prices{{table-suffix}} (
  foreign key (commodity_id) references commodities (id) on delete cascade,
  check (
    trade_date >= '{{start-of-period}}' and
    trade_date < '{{start-of-next-period}}'
  )
) inherits (prices_base);

create unique index if not exists uk_prices{{table-suffix}}_trade_date_commodity
  on prices{{table-suffix}} (trade_date, commodity_id);
