alter table budgets rename column period to period_type;
alter table budgets add column period text[];

update budgets b
set period = array[cast(s.period_count as text), s.period_type]
from budgets s
where b.id = s.id;

alter table budgets
  drop column period_type,
  drop column period_count,
  alter column period set not null;
