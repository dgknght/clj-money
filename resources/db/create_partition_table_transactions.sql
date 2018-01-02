create table if not exists transactions{{table-suffix}} (
  primary key (id),
  foreign key (entity_id) references entities on delete cascade,
  check (
    transaction_date >= '{{start-of-period}}' and
    transaction_date < '{{start-of-next-period}})'
  )
) inherits (transactions_base);

create index if not exists ix_transactions{{table-suffix}}_transaction_date
  on transactions{{table-suffix}} (transaction_date);
