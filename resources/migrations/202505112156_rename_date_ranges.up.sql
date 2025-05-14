alter table commodities add column if not exists price_date_range date[];

update commodities t
set price_date_range = array[s.earliest_price, s.latest_price]
from commodities s
where t.id = s.id
and s.earliest_price is not null;

alter table commodities drop column earliest_price;
alter table commodities drop column latest_price;
