create table {{table-name}} (
  check (
    trade_date >= {{start-of-period}} and
    trade_date < {{start-of-next-period}})
) inherits ({{base-table-name}});
