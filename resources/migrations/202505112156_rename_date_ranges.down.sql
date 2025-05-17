-- Entities
-----------
/* The up migration does not remove the data from settings, so I'm not addressing it here */
alter table entities drop column if exists price_date_range;
alter table entities drop column if exists transaction_date_range;

-- Commodities
--------------
alter table commodities add column if not exists earliest_price date;
alter table commodities add column if not exists latest_price date;

update commodities t
set earliest_price = s.price_date_range[1],
    latest_price   = s.price_date_range[2]
from commodities s
where t.id = s.id
and s.price_date_range is not null;

alter table commodities drop column price_date_range;

-- Accounts
--------------
alter table accounts add column if not exists earliest_transaction_date date;
alter table accounts add column if not exists latest_transaction_date date;

update accounts t
set earliest_transaction_date = s.transaction_date_range[1],
    latest_transaction_date   = s.transaction_date_range[2]
from accounts s
where t.id = s.id
and s.transaction_date_range is not null;

alter table accounts drop column transaction_date_range;
