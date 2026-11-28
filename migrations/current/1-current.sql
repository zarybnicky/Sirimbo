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
--!include functions/set_login_claims.sql
--!include functions/login.sql
--!include functions/otp_login.sql
--!include functions/register_using_invitation.sql
--!include functions/register_without_invitation.sql
--!include functions/get_current_user.sql
--!include functions/visible_person_ids.sql
--!include functions/can_edit_person.sql
--!include functions/competition_suggestions.sql
--!include policies/access_credential.sql
--!include functions/visible_announcement_ids.sql
--!include functions/queue_announcement_notification.sql
--!include views/person_last_activity.sql
--!include functions/global_search.sql
comment on column public.scoreboard_record.person_id is '@notNull';
comment on column public.scoreboard_record.lesson_total_score is '@notNull';
comment on column public.scoreboard_record.group_total_score is '@notNull';
comment on column public.scoreboard_record.event_total_score is '@notNull';
comment on column public.scoreboard_record.manual_total_score is '@notNull';
comment on column public.scoreboard_record.total_score is '@notNull';
comment on column public.scoreboard_record.ranking is '@notNull';
--!include functions/scoreboard.sql
--!include functions/trainer_group_attendance_completion.sql
--!include functions/federation_event_facets.sql
--!include functions/event_facets.sql
--!include functions/event_registration_info.sql
--!include functions/event_conflicts.sql
--!include views/crawler_frontier_summary.sql
--!include views/crawler_frontier_failure_group.sql
--!include functions/visible_file_ids.sql
--!include policies/file.sql
--!include policies/tenant.sql
--!include policies/tenant_membership.sql
--!include policies/tenant_trainer.sql
--!include policies/tenant_administrator.sql

drop type if exists announcement_audience_role;
drop view if exists current_tenant_membership;
drop view if exists current_tenant_trainer;
drop view if exists current_tenant_administrator;
drop view if exists current_cohort_membership;

alter table tenant
  add column if not exists accent_light text[],
  add column if not exists accent_dark text[];

alter table tenant drop constraint if exists tenant_accent_light_check;
alter table tenant add constraint tenant_accent_light_check check (
  cardinality(accent_light) = 12 and array_to_string(accent_light, '') !~ '[;{}<>\\]|/\*'
);
alter table tenant drop constraint if exists tenant_accent_dark_check;
alter table tenant add constraint tenant_accent_dark_check check (
  cardinality(accent_dark) = 12 and array_to_string(accent_dark, '') !~ '[;{}<>\\]|/\*'
);

grant update(accent_light) on table tenant to administrator;
grant update(accent_dark) on table tenant to administrator;

update tenant set accent_light = array[
    'hsl(0deg 100% 99%)',
    'hsl(7deg 100% 98%)',
    'hsl(5deg 100% 95%)',
    'hsl(6deg 100% 92%)',
    'hsl(5deg 100% 89%)',
    'hsl(4deg 100% 85%)',
    'hsl(3deg 98% 81%)',
    'hsl(3deg 88% 74%)',
    'hsl(352deg 86% 51%)',
    'hsl(350deg 100% 44%)',
    'hsl(350deg 100% 43%)',
    'hsl(352deg 89% 22%)'
] where id = 1 and accent_light is null;

update tenant set accent_light = array[
    'hsl(50deg 20% 99.1%)',
    'hsl(47deg 52.9% 96.7%)',
    'hsl(46deg 38.2% 93.7%)',
    'hsl(44deg 32.7% 90.1%)',
    'hsl(43deg 29.9% 85.7%)',
    'hsl(41deg 28.3% 79.8%)',
    'hsl(39deg 27.6% 71.9%)',
    'hsl(36deg 27.2% 61.8%)',
    'hsl(36deg 20% 49.5%)',
    'hsl(36deg 19.8% 45.7%)',
    'hsl(36deg 20% 39%)',
    'hsl(36deg 16% 20%)'
] where id in (2, 4) and accent_light is null;

update tenant set accent_light = array[
    'hsl(45deg 67% 99%)',
    'hsl(46deg 100% 96%)',
    'hsl(47deg 100% 88%)',
    'hsl(45deg 100% 83%)',
    'hsl(43deg 100% 77%)',
    'hsl(39deg 100% 73%)',
    'hsl(39deg 81% 68%)',
    'hsl(40deg 70% 56%)',
    'hsl(40deg 73% 58%)',
    'hsl(40deg 67% 53%)',
    'hsl(41deg 100% 31%)',
    'hsl(38deg 38% 20%)'
] where id = 3 and accent_light is null;

update tenant set accent_dark = array[
    'hsl(0deg 26% 7%)',
    'hsl(4deg 32% 10%)',
    'hsl(359deg 66% 15%)',
    'hsl(352deg 100% 17%)',
    'hsl(352deg 93% 21%)',
    'hsl(356deg 70% 28%)',
    'hsl(358deg 59% 37%)',
    'hsl(357deg 58% 48%)',
    'hsl(352deg 86% 51%)',
    'hsl(358deg 56% 51%)',
    'hsl(3deg 100% 77%)',
    'hsl(5deg 100% 90%)'
] where id = 1 and accent_dark is null;

update tenant set accent_dark = array[
    'hsl(44deg 9% 8.3%)',
    'hsl(43deg 14.3% 9.6%)',
    'hsl(42deg 15.5% 13%)',
    'hsl(41deg 16.4% 15.6%)',
    'hsl(41deg 16.9% 17.8%)',
    'hsl(40deg 17.6% 20.8%)',
    'hsl(38deg 18.5% 26.4%)',
    'hsl(36deg 19.6% 35.1%)',
    'hsl(36deg 20% 49.5%)',
    'hsl(36deg 22.3% 54.5%)',
    'hsl(35deg 30% 64%)',
    'hsl(49deg 52% 93.8%)'
] where id in (2, 4) and accent_dark is null;

update tenant set accent_dark = array[
    'hsl(43deg 23% 6%)',
    'hsl(38deg 24% 9%)',
    'hsl(39deg 48% 11%)',
    'hsl(41deg 87% 12%)',
    'hsl(41deg 94% 14%)',
    'hsl(41deg 77% 19%)',
    'hsl(40deg 69% 24%)',
    'hsl(41deg 75% 30%)',
    'hsl(40deg 73% 58%)',
    'hsl(40deg 67% 53%)',
    'hsl(39deg 78% 61%)',
    'hsl(38deg 78% 85%)'
] where id = 3 and accent_dark is null;

comment on column aktuality.at_kat is '@behavior -*';

--!include functions/save_events.sql

comment on column public.person.id is '@behavior filterType:eq filterType:eqIn';
comment on column public.posting.account_id is '@behavior filterType:eq filterType:eqIn';
comment on column public.transaction.source is '@behavior filterType:eq filterType:eqIn';
comment on column federated.competitor.competitor_type is '@behavior filterType:eq filterType:eqIn';

comment on column federated.person.age_group_code is '@name ageGroup
@behavior -insert -update filterType:eq filterType:eqIn';
comment on column federated.person.gender is '@behavior filterType:eq filterType:eqIn';
comment on column federated.person.nationality is '@behavior filterType:eq filterType:eqIn';
--!include functions/federation_people_facets.sql

comment on column public.event.type is '@behavior filterType:eq filterType:eqIn';
comment on column public.series_info.id is '@notNull';
comment on column public.series_info.position is '@notNull';
comment on column public.series_info.length is '@notNull';
comment on column public.series_info.since is '@notNull';
comment on column public.series_info.until is '@notNull';
comment on column public.event.location_id is '@behavior filterType:eq filterType:eqIn';
comment on column public.event_trainer.person_id is '@behavior filterType:eq filterType:eqIn';
comment on column public.payment_debtor.person_id is '@behavior filterType:eq filterType:eqIn';
comment on table public.event is '@behavior +list -connection -insert
@ref trainers via:public.event_trainer behavior:"filterable -list -connection -single"';
comment on table public.payment is '@behavior +list +connection -insert -query:resource:list +query:resource:connection
@ref debtors via:public.payment_debtor behavior:"filterable -list -connection -single"';
comment on table public.transaction is '@behavior -insert -update
@ref postings via:public.posting behavior:"filterable -list -connection -single"';
