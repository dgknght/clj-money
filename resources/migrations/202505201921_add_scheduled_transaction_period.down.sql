alter table scheduled_transactions
  add column if not exists interval_type varchar(20),
  add column if not exists interval_count int;

update scheduled_transactions t
set interval_type = s.period[2],
    interval_count = cast(s.period[1] as int)
from scheduled_transactions s
where t.id = s.id;

alter table scheduled_transactions drop column if exists period;
alter table scheduled_transactions
  alter column interval_type set not null,
  alter column interval_count set not null
