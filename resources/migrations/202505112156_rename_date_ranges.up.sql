-- Entities
-----------
alter table entities add column if not exists price_date_range date[];
alter table entities add column if not exists transaction_date_range date[];

update entities t set
  price_date_range = array[
    TO_DATE(s.settings->>'settings/earliest-price-date', 'YYYY-MM-DD'),
    TO_DATE(s.settings->>'settings/latest-price-date', 'YYYY-MM-DD')
  ]
from entities s
where t.id = s.id
and s.settings->>'settings/earliest-price-date' is not null;

update entities t set
  transaction_date_range = array[
    TO_DATE(s.settings->>'settings/earliest-transaction-date', 'YYYY-MM-DD'),
    TO_DATE(s.settings->>'settings/latest-transaction-date', 'YYYY-MM-DD')
  ]
from entities s
where t.id = s.id
and s.settings->>'settings/earliest-transaction-date' is not null;

-- Commodities
--------------
alter table commodities add column if not exists price_date_range date[];

update commodities t
set price_date_range = array[s.earliest_price, s.latest_price]
from commodities s
where t.id = s.id
and s.earliest_price is not null;

alter table commodities drop column earliest_price;
alter table commodities drop column latest_price;
