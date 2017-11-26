create table if not exists transaction_items{{table-suffix}} (
  primary key (id),
  foreign key (transaction_id) references transactions{{table-suffix}} on delete cascade,
  foreign key (account_id) references accounts on delete cascade,
  check (
    transaction_date >= '{{start-of-period}}' and
    transaction_date < '{{start-of-next-period}}'
  )
) inherits (transaction_items_base);

create index if not exists ix_transaction_items{{table-suffix}}_transaction_date
  on transaction_items{{table-suffix}} (transaction_date);

create index if not exists ix_transaction_items{{table-suffix}}_transaction_id
  on transaction_items{{table-suffix}} (transaction_id);

create index if not exists ix_transaction_items{{table-suffix}}_account_id
  on transaction_items{{table-suffix}} (account_id);
