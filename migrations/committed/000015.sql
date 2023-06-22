--! Previous: sha1:f85656ff2cb70c6dfd7a7b9b41c0ac47e90a4a8a
--! Hash: sha1:25e6b75e75d5ed623ca7fb3b066518feda5d9211

create or replace function my_announcements() returns setof upozorneni as $$
  select upozorneni.* from upozorneni
  where is_visible = true and sticky = false
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by up_timestamp_add desc;
$$ language sql stable;
grant execute on function my_announcements to anonymous;

create or replace function sticky_announcements() returns setof upozorneni as $$
  select upozorneni.* from upozorneni
  where is_visible = true and sticky = true
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by up_timestamp_add desc;
$$ language sql stable;
grant execute on function sticky_announcements to anonymous;

create or replace function archived_announcements() returns setof upozorneni as $$
  select upozorneni.* from upozorneni
  where is_visible = false
    or (scheduled_until is null or scheduled_until >= now())
  order by up_timestamp_add desc;
$$ language sql stable;
grant execute on function archived_announcements to anonymous;

alter table attachment alter column uploaded_by set default current_user_id();

create or replace function attachment_directory(attachment attachment) returns text AS $$
  SELECT regexp_replace(attachment.object_name, '/[^/]*$', '');
$$ language SQL STABLE;
grant execute on function attachment_directory(attachment) to anonymous;
comment on function attachment_directory(attachment) is E'@filterable';

create or replace function attachment_directories() returns setof text AS $$
  SELECT distinct regexp_replace(object_name, '/[^/]*$', '') from attachment;
$$ language SQL STABLE;
grant execute on function attachment_directories to anonymous;
comment on function attachment_directories is E'@sortable';
