alter table scheduled_transactions add column if not exists period text[];

update scheduled_transactions t
set period = array[cast(s.interval_count as text), s.interval_type]
from scheduled_transactions s
where t.id = s.id;

alter table scheduled_transactions
    drop column if exists interval_count,
    drop column if exists interval_type,
    alter column period set not null
