create table {{table-name}} (
  check (
    transaction_date >= {{start-of-period}} and
    transaction_date < {{start-of-next-period}})
) inherits ({{base-table-name}});
