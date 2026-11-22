lock table tenant_membership, tenant_trainer, tenant_administrator, cohort_membership, event_registration, event_trainer
  in share row exclusive mode;

do $$ begin
  if to_regtype('app_private.tenant_role') is not null then
    alter type app_private.tenant_role set schema public;
    comment on type tenant_role is null;
  end if;
  if to_regtype('public.tenant_role') is null then
    create type tenant_role as enum ('member', 'trainer', 'administrator');
  end if;
end $$;

do $$ begin
  if exists (
    select from pg_attribute
    where attrelid = 'announcement_audience'::regclass and attname = 'audience_role'
      and atttypid <> 'public.tenant_role'::regtype
  ) then
    alter table announcement_audience alter column audience_role
      type tenant_role using audience_role::text::tenant_role;
  end if;
end $$;

do $$ begin
  if exists (
    select from pg_attribute
    where attrelid = (select typrelid from pg_type where oid = 'public.jwt_token'::regtype)
      and attname = 'guest_tenant_ids' and not attisdropped
  ) then
    alter type public.jwt_token drop attribute guest_tenant_ids;
  end if;
end $$;

create table if not exists app_private.tenant_relation (
  tenant_id bigint not null references tenant(id) on delete cascade,
  person_id bigint not null references person(id) on delete cascade,
  current_roles tenant_role[] not null default '{}',
  expired_roles tenant_role[] not null default '{}',
  pending_roles tenant_role[] not null default '{}',
  current_cohort_ids bigint[] not null default '{}',
  expired_cohort_ids bigint[] not null default '{}',
  has_event_participation boolean not null default false,
  has_roles boolean generated always as (current_roles <> '{}' or expired_roles <> '{}' or pending_roles <> '{}') stored,
  is_current boolean generated always as (current_roles <> '{}') stored,
  is_former boolean generated always as (current_roles = '{}' and expired_roles <> '{}') stored,
  primary key (tenant_id, person_id)
);

alter table app_private.tenant_relation
  add column if not exists has_event_participation boolean not null default false;

create index if not exists tenant_relation_person_idx
  on app_private.tenant_relation (person_id, tenant_id);
create index if not exists tenant_relation_current_idx
  on app_private.tenant_relation (tenant_id, person_id) include (current_roles)
  where is_current;

grant select on app_private.tenant_relation to anonymous;

drop trigger if exists _500_on_status on tenant_membership;
drop trigger if exists _600_tenant_relation_update on tenant_membership;
drop trigger if exists _500_on_status on tenant_trainer;
drop trigger if exists _600_tenant_relation_update on tenant_trainer;
drop trigger if exists _500_on_status on tenant_administrator;
drop trigger if exists _600_tenant_relation_update on tenant_administrator;
drop trigger if exists _500_on_status_update on cohort_membership;
drop trigger if exists _600_tenant_relation on cohort_membership;
drop trigger if exists _600_tenant_relation_update on cohort_membership;
drop function if exists app_private.tg_tenant_membership__on_status();

--!include functions/tenant_relation.sql
--!include functions/tg_event_people__refresh.sql
--!include functions/cron_update_memberships.sql
--!include functions/tg_cohort_membership__refresh.sql
--!include functions/security_events.sql

select app_private.rebuild_tenant_relation(tenant_id, person_id)
from (
  select tenant_id, person_id from tenant_membership
  union select tenant_id, person_id from tenant_trainer
  union select tenant_id, person_id from tenant_administrator
  union select tenant_id, person_id from cohort_membership
  union select tenant_id, person_id from event_registration where person_id is not null and registration_status = 'active'
  union select tenant_id, person_id from event_trainer
  union select tenant_id, person_id from app_private.tenant_relation
) pairs order by tenant_id, person_id;

--!include functions/upsert_announcement.sql
--!include views/current_tenant_relation.sql
--!include functions/person_facets.sql
--!include functions/create_jwt_token.sql
--!include functions/visible_person_ids.sql
--!include functions/can_edit_person.sql
--!include functions/competition_suggestions.sql
--!include policies/access_credential.sql
--!include functions/visible_announcement_ids.sql
--!include functions/queue_announcement_notification.sql
--!include views/person_last_activity.sql
--!include functions/global_search.sql
--!include functions/scoreboard.sql
--!include functions/event_facets.sql
--!include functions/event_conflicts.sql
--!include functions/visible_file_ids.sql
--!include policies/file.sql

drop type if exists announcement_audience_role;
drop view if exists current_tenant_membership;
drop view if exists current_tenant_trainer;
drop view if exists current_tenant_administrator;
drop view if exists current_cohort_membership;
