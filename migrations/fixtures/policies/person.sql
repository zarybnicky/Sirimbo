select app_private.drop_policies('public.person');
create policy admin_all on person to administrator using (true);
create policy admin_myself on person for update using (id = any (current_person_ids()));
create policy view_tenant_or_trainer on person for select using ((
    select (select current_tenant_id()) = any (allowed_tenants) and (
         (select current_tenant_id()) in (select my_tenant_ids())
      or (select current_tenant_id()) = any (tenant_trainers)
      or (select current_tenant_id()) = any (tenant_administrators)
    )
    from auth_details where person_id=id
));
