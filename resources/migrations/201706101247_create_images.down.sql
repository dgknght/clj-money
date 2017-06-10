drop table images;

alter table imports
  drop column image_id,
  add column content bytea not null;
