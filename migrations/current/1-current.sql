revoke all on table tenant from anonymous;
grant select on table tenant to anonymous;
grant update (name, description, bank_account, cz_ico, cz_dic, address) on table tenant to administrator;
grant all on table tenant to system_admin;

drop trigger if exists _200_refresh_auth_details on cohort_membership;
drop trigger if exists _200_refresh_auth_details on couple;
drop trigger if exists _200_refresh_auth_details on tenant_membership;
drop trigger if exists _200_refresh_auth_details on tenant_trainer;
drop trigger if exists _200_refresh_auth_details on tenant_administrator;
drop function if exists app_private.tg_auth_details__refresh();

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

alter table access_credential
  add column if not exists status relationship_status not null default 'active';

update access_credential
set status = app_private.relationship_status_next(now(), valid_range, status)
where status is distinct from app_private.relationship_status_next(now(), valid_range, status);

create table if not exists security_event (
  id bigint generated always as identity primary key,
  tenant_id bigint not null default current_tenant_id() references tenant (id) on delete cascade,
  user_id bigint references users (id) on delete set null,
  person_id bigint references person (id) on delete set null,
  actor_user_id bigint default current_user_id() references users (id) on delete set null,
  kind text not null,
  method text not null check (method in ('password', 'otp', 'manual', 'scheduled')),
  occurred_at timestamptz not null default now(),
  effective_at timestamptz not null default now()
);

create index if not exists security_event_tenant_occurred_at_idx
  on security_event (tenant_id, occurred_at desc);
create index if not exists security_event_user_occurred_at_idx
  on security_event (user_id, occurred_at desc);
create index if not exists security_event_person_occurred_at_idx
  on security_event (person_id, occurred_at desc);
create index if not exists security_event_actor_user_id_idx
  on security_event (actor_user_id);

comment on table security_event is '@omit create,update,delete
@simpleCollections only
@behavior -query:resource:list -query:resource:connection -query:resource:single';
comment on constraint security_event_tenant_id_fkey on security_event is
  '@behavior -manyRelation:resource:list -manyRelation:resource:connection';
comment on constraint security_event_actor_user_id_fkey on security_event is
  '@behavior -manyRelation:resource:list -manyRelation:resource:connection';

alter table security_event enable row level security;
grant select on table security_event to member;

select app_private.drop_policies('public.security_event');
create policy current_tenant on security_event as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy self_view on security_event for select to member
  using (
    user_id = (select current_user_id())
    or person_id = any ((select current_person_ids())::bigint[])
  );
create policy admin_view on security_event for select to administrator using (true);

--!include functions/cron_update_memberships.sql
--!include functions/security_events.sql
--!include functions/login.sql
--!include functions/otp_login.sql
--!include functions/reset_password.sql
--!include functions/change_password.sql
--!include functions/register_without_invitation.sql
--!include functions/register_using_invitation.sql
--!include functions/log_in_as.sql
--!include functions/confirm_membership_application.sql

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
