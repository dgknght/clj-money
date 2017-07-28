alter table images add column content_type varchar(100);

update images set content_type = a.content_type
from attachments a where a.image_id = images.id;
update images set content_type = 'application/gnucash'
where content_type is null;

alter table attachments drop column content_type;
alter table images alter column content_type set not null;
