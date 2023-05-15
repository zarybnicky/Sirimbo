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
