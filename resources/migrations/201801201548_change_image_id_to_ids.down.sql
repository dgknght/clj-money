update imports add column image_id integer;
update imports set image_id = image_ids[0];
alter table imports
  drop column image_ids,
  alter column image_id set not null;
