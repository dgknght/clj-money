alter table commodities add column if not exists earliest_price date;
alter table commodities add column if not exists latest_price date;

update commodities t
set earliest_price = s.price_date_range[1],
    latest_price   = s.price_date_range[2]
from commodities s
where t.id = s.id
and s.price_date_range is not null;

alter table commodities drop column price_date_range;
