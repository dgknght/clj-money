alter table imports add column image_ids integer[];
update imports set image_ids = array[image_id];
alter table imports
  drop column image_id,
  alter column image_ids set not null;
