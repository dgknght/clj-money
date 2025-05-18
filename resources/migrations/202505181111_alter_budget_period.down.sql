alter table budgets
  add column if not exists period_type varchar(20),
  add column if not exists period_count smallint;

update budgets b
set period_type = s.period[2],
    period_count = cast(s.period[1] as smallint)
from budgets s
where b.id = s.id;

alter table budgets drop column period;
alter table budgets rename column period_type to period;
alter table budgets
  alter column period set not null,
  alter column period_count set not null;
