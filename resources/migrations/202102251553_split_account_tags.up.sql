alter table accounts rename column tags to system_tags;
alter table accounts add column user_tags text[];
