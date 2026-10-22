do $$
begin
  if not exists (
    select 1 from pg_catalog.pg_attribute
    where attrelid = 'public.aktuality'::regclass
      and attname = 'published_at'
      and not attisdropped
  ) then
    alter table aktuality add column published_at timestamptz;

    alter table aktuality disable trigger _100_timestamps;
    alter table aktuality disable trigger _200_author;
    update aktuality set published_at = coalesce(updated_at, created_at) where published_at is null;
    alter table aktuality enable trigger _100_timestamps;
    alter table aktuality enable trigger _200_author;
  end if;
end;
$$;

alter table aktuality alter column published_at set default now();

alter table aktuality
  drop constraint if exists aktuality_visible_requires_published_at,
  add constraint aktuality_visible_requires_published_at
  check (not is_visible or published_at is not null);

grant execute on function app_private.announcement_status_next to anonymous;

--!include functions/upsert_article.sql

alter table event_instance_registration
  add column if not exists created_by bigint references users(id) on delete set null;
alter table event_instance_registration alter column created_by set default current_user_id();

comment on column event_instance_registration.created_by is '@omit';
comment on constraint event_instance_registration_created_by_fkey on event_instance_registration is '@omit';

alter table person_invitation add column if not exists instance_id bigint references event_instance(id) on delete set null;

comment on column person_invitation.instance_id is '@omit';
comment on constraint person_invitation_instance_id_fkey on person_invitation is '@omit';

--!include functions/ensure_couple_person_registrations.sql
--!include functions/save_events.sql
--!include functions/set_event_instance_registration.sql
--!include functions/set_lesson_demand.sql
--!include functions/event_remaining_x.sql
--!include functions/event_share_claims.sql

--!include views/event_people.sql
--!include functions/accessible_event_ids.sql
--!include functions/tg_event_people__refresh.sql
--!include functions/visible_person_ids.sql

--!include policies/event_instance.sql
--!include policies/event_instance_trainer.sql
--!include policies/event_instance_target_cohort.sql
--!include policies/cohort.sql
--!include policies/event_instance_registration.sql
--!include policies/event_lesson_demand.sql
--!include policies/file.sql
--!include policies/person.sql
--!include policies/couple.sql
--!include policies/access_event.sql
--!include policies/accounting_period.sql
--!include policies/cohort_group.sql
--!include policies/cohort_membership.sql
--!include policies/cohort_subscription.sql
--!include policies/dokumenty.sql
--!include policies/form_responses.sql
--!include policies/location.sql
--!include policies/location_image.sql
--!include policies/person_invitation.sql
--!include policies/scoreboard_manual_adjustment.sql
--!include policies/security_event.sql
--!include policies/tenant.sql
--!include policies/tenant_administrator.sql
--!include policies/tenant_membership.sql
--!include policies/tenant_settings.sql
--!include policies/tenant_trainer.sql
--!include policies/aktuality.sql

alter table otp_token
  add column if not exists intent text not null default 'password_reset',
  add column if not exists event_id bigint references event_instance(id) on delete cascade;

alter table otp_token drop constraint if exists otp_token_intent_check;
alter table otp_token add constraint otp_token_intent_check check (
  (intent = 'password_reset' and event_id is null)
  or (intent = 'event_handoff' and event_id is not null)
);

--!include functions/otp_login.sql

do $$
begin
  if to_regclass('public.event_external_registration') is not null then
    lock table public.event_external_registration in access exclusive mode;
    if exists (select 1 from public.event_external_registration) then
      raise exception 'Convert external registrations with the manual script before removing the table';
    end if;
  end if;
end;
$$;

--!include functions/event_instance_registration_info.sql

drop table if exists public.event_external_registration;

alter table federated.person
  add column if not exists birth_year integer,
  add column if not exists national_reference text;

comment on column federated.person.national_reference is '@behavior -*';

create index if not exists federated_person_wdsf_national_reference_idx
  on federated.person (national_reference) where federation = 'wdsf';

update federated.person p
set birth_year = nullif((c.content->>'yearOfBirth')::integer, 0),
    national_reference = nullif(c.content->>'nationalReference', '')
from crawler.frontier f
join crawler.json_response r on r.id = f.last_successful_response_id
join crawler.json_response_cache c using (content_hash)
where f.federation = 'wdsf' and f.kind = 'member'
  and p.federation = 'wdsf' and p.external_id::text = f.key
  and (p.birth_year, p.national_reference) is distinct from
    (nullif((c.content->>'yearOfBirth')::integer, 0), nullif(c.content->>'nationalReference', ''));

--!include functions/person_federation_candidates.sql

alter table federated.competitor
  add column if not exists latest_result_at date,
  add column if not exists latest_entry_at date;

--!include functions/refresh_competitor_activity.sql

with results as (
  select r.competitor_id, max(c.start_date) as latest_result_at
  from federated.competition_result r
  join federated.competition c on c.id = r.competition_id
  group by r.competitor_id
), entries as (
  select e.competitor_id, max(c.start_date) as latest_entry_at
  from federated.competition_entry e
  join federated.competition c on c.id = e.competition_id
  where not e.cancelled and not exists (
    select 1 from federated.competition_result r
    where (r.competition_id, r.competitor_id) = (e.competition_id, e.competitor_id)
  )
  group by e.competitor_id
), activity as (
  select c.id, r.latest_result_at, e.latest_entry_at
  from federated.competitor c
  left join results r on r.competitor_id = c.id
  left join entries e on e.competitor_id = c.id
)
update federated.competitor c
set latest_result_at = a.latest_result_at, latest_entry_at = a.latest_entry_at
from activity a
where c.id = a.id
  and (c.latest_result_at, c.latest_entry_at) is distinct from
    (a.latest_result_at, a.latest_entry_at);

do $$
begin
  if not exists (select 1 from pg_catalog.pg_constraint where conrelid = 'public.file'::regclass and conname = 'file_tenant_id_id_key') then
    alter table file
      add constraint file_tenant_id_id_key unique using index file_tenant_id_id_idx;
  end if;
end;
$$;

comment on constraint file_tenant_id_id_key on file
  is '@behavior -query:resource:single';

--!include functions/create_credit_transaction_for_person.sql
--!include functions/upsert_announcement.sql
--!include functions/upsert_location.sql
--!include functions/confirm_membership_application.sql
--!include functions/create_person.sql
--!include functions/move_event_instance.sql
--!include functions/update_attendance.sql
--!include functions/federation_people_facets.sql
--!include functions/federation_competitor_facets.sql
--!include functions/federation_event_facets.sql
--!include views/competition_participation.sql

comment on table federated.category is '@name category
@behavior -insert -update -delete +list -connection';
comment on table federated.federation is '@name federation
@behavior -insert -update -delete +list -connection';
comment on table federated.competition is '@name competition
@behavior -insert -update -delete -list';
comment on table federated.ranklist_entry is '@name ranklist_entry
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
comment on table federated.competition_entry is '@name competition_entry
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
comment on table federated.competition_official is '@name competition_official
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
comment on table federated.competition_result is '@name competition_result
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
comment on table federated.competition_round is '@name competition_round
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list';
comment on table federated.competition_round_judge is '@name competition_round_judge
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
comment on table federated.competition_round_result is '@name competition_round_result
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
comment on table federated.dance is '@name dance
@behavior -insert -update -delete +list -connection';
comment on table federated.dance_program is '@name dance_program
@behavior -insert -update -delete +list -connection';
comment on table federated.dance_program_dance is '@name dance_program_dance
@behavior -insert -update -delete +list -connection';
comment on table federated.federation_club is '@name federation_club
@behavior -insert -update -delete -list';
comment on table federated.judge_score is '@name judge_score
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';
comment on table federated.ranklist is '@name ranklist
@behavior -insert -update -delete -list';
comment on table federated.ranklist_snapshot is '@name ranklist_snapshot
@behavior -insert -update -delete -list -query:resource:connection -query:resource:list -query:resource:single';

comment on type federated.competitor_role is '@name competitor_role';
comment on type federated.gender is '@name gender';
comment on type federated.person_license_discipline is '@name person_license_discipline';
comment on type federated.person_license_kind is '@name person_license_kind';
comment on type federated.person_license_status is '@name person_license_status';
comment on type federated.competition_type is '@name competition_type';
comment on type federated.competitor_type is '@name competitor_type';
comment on type federated.official_role is '@name official_role';
comment on type federated.score_component is '@name score_component';
comment on type federated.scoring_method is '@name scoring_method';
comment on function set_event_sharing is '@resultFieldName string';
comment on function update_tenant_settings_key is '@resultFieldName tenantSetting';

comment on column federated.event_official.role is '@behavior +attribute:orderBy';
comment on column federated.competition_official.role is '@behavior +attribute:orderBy';
comment on column federated.person_license.kind is '@behavior +attribute:orderBy';
comment on column federated.person_license.discipline is '@behavior +attribute:orderBy';
comment on column federated.judge_score.component is '@behavior +attribute:orderBy';
comment on column federated.competitor_component.role is '@behavior +attribute:orderBy';

--!include views/person_last_activity.sql
