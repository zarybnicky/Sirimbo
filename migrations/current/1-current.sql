--!include functions/register_using_invitation.sql
--!include functions/confirm_membership_application.sql

revoke all on table tenant from anonymous;
grant select on table tenant to anonymous;
grant update (name, description, bank_account, cz_ico, cz_dic, address) on table tenant to administrator;
grant all on table tenant to system_admin;

select app_private.drop_policies('public.tenant');
create policy system_admin_all on tenant to system_admin using (true);
create policy admin_all on tenant to administrator using (id = (select current_tenant_id()));
create policy public_view on tenant for select to anonymous using (id = (select current_tenant_id()));

comment on table tenant is '@omit create,delete
@behavior -singularRelation:resource:single -query:resource:connection
@simpleCollections only';

comment on table tenant_membership is '@simpleCollections both
@behavior -query:resource:list -query:resource:connection';
comment on table tenant_trainer is '@simpleCollections both
@behavior -query:resource:list -query:resource:connection';
comment on table tenant_administrator is '@simpleCollections both
@behavior -query:resource:list -query:resource:connection';
comment on table tenant_settings is '@omit create,delete
@behavior -query:resource:list -query:resource:connection -singularRelation:resource:list';

alter table access_event add column if not exists location_id bigint;

do $$
begin
  if not exists (
    select 1
    from pg_constraint
    where conname = 'access_event_location_fkey'
      and conrelid = 'access_event'::regclass
  ) then
    alter table access_event
      add constraint access_event_location_fkey
      foreign key (tenant_id, location_id)
      references tenant_location (tenant_id, id);
  end if;
end;
$$;

update access_event e
set location_id = 12
where e.location_id is null
  and e.device = 'ASI1201E-V1'
  and exists (
    select 1
    from tenant_location l
    where l.tenant_id = e.tenant_id
      and l.id = 12
  );

create index if not exists access_event_location_idx
  on access_event (tenant_id, location_id, occurred_at desc);

select app_private.drop_policies('public.tenant_settings');
create policy system_admin_all on tenant_settings to system_admin using (true);
create policy admin_own on tenant_settings to administrator using (tenant_id = (select current_tenant_id()));

select app_private.drop_policies('public.tenant_administrator');
create policy public_view on tenant_administrator for select using (true);
create policy system_admin_all on tenant_administrator to system_admin using (true);
create policy admin_all on tenant_administrator to administrator using (tenant_id = (select current_tenant_id()));

select app_private.drop_policies('public.tenant_trainer');
create policy public_view on tenant_trainer for select using (true);
create policy system_admin_all on tenant_trainer to system_admin using (true);
create policy admin_all on tenant_trainer to administrator using (tenant_id = (select current_tenant_id()));

select app_private.drop_policies('public.tenant_membership');
create policy view_visible_person on tenant_membership for select using (true);
create policy system_admin_all on tenant_membership to system_admin using (true);
create policy admin_all on tenant_membership to administrator using (tenant_id = (select current_tenant_id()));
