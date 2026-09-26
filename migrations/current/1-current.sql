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
