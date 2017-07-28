alter table attachments add column content_type varchar(100);

update attachments set content_type = i.content_type
from images i where i.id = attachments.image_id;

alter table images drop column content_type;
alter table attachments alter column content_type set not null;
