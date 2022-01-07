alter table commodities add column price_config jsonb;

update commodities
set price_config = '{"enabled": false}';

update commodities
set price_config = '{"enabled": true}'
from lots l
where l.commodity_id = commodities.id and l.shares_owned > 0;

alter table commodities alter column price_config set not null;
