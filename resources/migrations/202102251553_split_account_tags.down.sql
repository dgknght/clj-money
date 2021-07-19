alter table accounts drop column user_tags;
alter table accounts rename column system_tags to tags;
