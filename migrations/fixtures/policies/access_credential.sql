select app_private.drop_policies('public.access_credential');

create policy current_tenant on access_credential as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_view on access_credential for select to administrator using (true);
create policy admin_insert on access_credential for insert to administrator with check (
  exists (select 1 from tenant_membership r where r.tenant_id = access_credential.tenant_id and r.person_id = access_credential.person_id)
  or exists (select 1 from tenant_trainer r where r.tenant_id = access_credential.tenant_id and r.person_id = access_credential.person_id)
  or exists (select 1 from tenant_administrator r where r.tenant_id = access_credential.tenant_id and r.person_id = access_credential.person_id)
);
create policy admin_update on access_credential for update to administrator using (true) with check (
  exists (select 1 from tenant_membership r where r.tenant_id = access_credential.tenant_id and r.person_id = access_credential.person_id)
  or exists (select 1 from tenant_trainer r where r.tenant_id = access_credential.tenant_id and r.person_id = access_credential.person_id)
  or exists (select 1 from tenant_administrator r where r.tenant_id = access_credential.tenant_id and r.person_id = access_credential.person_id)
);

grant all on table access_credential to anonymous;
