alter table images add column if not exists body bytea;
alter table images rename column uuid to body_hash;

update images i
set body = c.content
from image_contents c
where i.body_hash = c.uuid
and i.body is null;

alter table images alter column body set not null;
drop table if exists image_contents;
drop sequence if exists public.image_contents_id_seq;
