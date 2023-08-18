-- Write your migration here

comment on table person is E'@omit create,delete';


select app_private.drop_policies('public.person');
create policy admin_all on person to administrator using (true);
create policy admin_myself on person for update using (id in (select my_person_ids()));
create policy view_same_tenant on person for select using (exists (select 1 from tenant_membership where active=true and person_id = id and tenant_id in (select my_tenant_ids())));
create policy view_tenant_admin on person for select using (exists (select 1 from tenant_administrator where active=true and person_id = id));
create policy view_tenant_trainer on person for select using (exists (select 1 from tenant_trainer where active=true and person_id = id));
