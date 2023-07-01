--! Previous: sha1:25e6b75e75d5ed623ca7fb3b066518feda5d9211
--! Hash: sha1:82aad5d0439232ad52ea3b6646e76dbfc6765c76

--! split: 01-olymp.sql
create or replace function event_is_future(event event) returns boolean AS $$
  SELECT event.until >= now();
$$ language SQL STABLE;
grant execute on function event_is_future(event) to anonymous;
comment on function event_is_future(event) is E'@filterable';

alter table attachment add column if not exists thumbhash text null;
alter table attachment add column if not exists width int null;
alter table attachment add column if not exists height int null;

drop function if exists my_announcements();
create or replace function my_announcements(archive boolean default false) returns setof upozorneni as $$
  select upozorneni.* from upozorneni
  where is_visible = not archive and sticky = false
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by up_timestamp_add desc;
$$ language sql stable;
grant execute on function my_announcements to anonymous;

alter table event add column if not exists description_member text not null default '';

alter table event add column if not exists title_image_legacy text null default null;
