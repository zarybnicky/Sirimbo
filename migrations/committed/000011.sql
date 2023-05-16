--! Previous: sha1:9c7127f1e73ce8f6e621635d319eea2ad1430ac6
--! Hash: sha1:77934dcafea4059827bff9cf44efb4c43e6fbe47

alter table event alter column files_legacy set default '';
alter table platby_category alter column pc_use_base set default false;

select app_private.drop_policies('public.users');
alter table users enable row level security;
grant all on users to anonymous;
create policy admin_all on users to administrator using (true) with check (true);
create policy all_view on users for select to member using (true);
create policy manage_own on users for all
  using (u_id = current_user_id()) with check (u_id = current_user_id());
create policy register_anonymous on users for insert with check (u_confirmed=false);


create or replace function app_private.tg__tenant() returns trigger as $$
begin
  NEW.tenant = (case when TG_OP = 'INSERT' then get_current_tenant() else OLD.tenant end);
  return NEW;
end;
$$ language plpgsql volatile;

alter table upozorneni add column if not exists is_visible boolean default true;

create or replace function my_announcements() returns setof upozorneni as $$
  select upozorneni.* from upozorneni
  where is_visible = true
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by up_timestamp_add desc;
$$ language sql stable;
grant execute on function my_announcements to member;

alter table if exists video set schema app_private;
alter table if exists video_list set schema app_private;
alter table if exists video_source set schema app_private;

select app_private.drop_policies('public.person');
alter table person enable row level security;
grant all on person to anonymous;
revoke all on person from administrator;
create policy manage_admin on person to administrator using (true) with check (true);
create policy view_all on person for select using (true);
