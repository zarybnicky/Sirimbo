do $$
declare
  renamed_table record;
  renamed_column record;
  object_name text;
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

  if to_regtype('public.event_instance_registration_status') is not null then
    alter type event_instance_registration_status rename to event_registration_status;
  end if;

  for renamed_table in
    select * from (values
      ('event_instance_target_cohort', 'event_target_cohort'),
      ('event_instance_trainer', 'event_trainer'),
      ('event_lesson_demand', 'event_lesson_request'),
      ('event_instance_registration', 'event_registration'),
      ('event_instance', 'event')
    ) names(old_name, new_name)
  loop
    if to_regclass('public.' || renamed_table.old_name) is null then
      continue;
    end if;

    execute format('alter table public.%I rename to %I', renamed_table.old_name, renamed_table.new_name);
    if starts_with(renamed_table.old_name, 'event_instance_') then
      execute format('alter table public.%I rename column instance_id to event_id', renamed_table.new_name);
    end if;

    for object_name in
      select conname from pg_catalog.pg_constraint
      where conrelid = to_regclass('public.' || renamed_table.new_name)
        and starts_with(conname::text, renamed_table.old_name || '_')
    loop
      execute format('alter table public.%I rename constraint %I to %I',
        renamed_table.new_name, object_name,
        replace(replace(object_name, renamed_table.old_name, renamed_table.new_name), 'instance_id', 'event_id'));
    end loop;

    -- Constraint-backed indexes follow their constraints; rename the remaining indexes.
    for object_name in
      select indexname from pg_catalog.pg_indexes
      where schemaname = 'public' and tablename = renamed_table.new_name
        and starts_with(indexname::text, renamed_table.old_name || '_')
    loop
      execute format('alter index public.%I rename to %I', object_name,
        replace(replace(replace(object_name, renamed_table.old_name, renamed_table.new_name),
          'instance_id', 'event_id'), '_instance_idx', '_event_idx'));
    end loop;

    execute format('alter sequence public.%I rename to %I',
      renamed_table.old_name || '_id_seq', renamed_table.new_name || '_id_seq');

    if renamed_table.new_name = 'event_lesson_request' then
      alter table event_lesson_request rename constraint eld_unique_registration_trainer_key
        to event_lesson_request_registration_id_trainer_id_key;
    end if;
  end loop;
end;
$$;

alter table aktuality alter column published_at set default now();

alter table aktuality
  drop constraint if exists aktuality_visible_requires_published_at,
  add constraint aktuality_visible_requires_published_at
  check (not is_visible or published_at is not null);

grant execute on function app_private.announcement_status_next to anonymous;

--!include functions/upsert_article.sql

alter table event_registration
  add column if not exists created_by bigint references users(id) on delete set null;
alter table event_registration alter column created_by set default current_user_id();

comment on column event_registration.created_by is '@omit';
comment on constraint event_registration_created_by_fkey on event_registration is '@omit';

do $$
begin
  if exists (
    select 1 from pg_catalog.pg_attribute
    where attrelid = 'public.person_invitation'::regclass
      and attname = 'instance_id' and not attisdropped
  ) then
    alter table person_invitation rename column instance_id to event_id;
    alter table person_invitation rename constraint person_invitation_instance_id_fkey to person_invitation_event_id_fkey;
  end if;
end;
$$;

alter table person_invitation add column if not exists event_id bigint references event(id) on delete set null;

comment on column person_invitation.event_id is '@omit';
comment on constraint person_invitation_event_id_fkey on person_invitation is '@omit';

--!include functions/reconcile_event_cohort_registrations.sql
--!include functions/tg_cohort_membership__on_status.sql
--!include functions/can_trainer_edit_event.sql
--!include functions/tg_event__refresh_manager_person_ids.sql
--!include functions/ensure_couple_person_registrations.sql
--!include functions/save_events.sql
--!include functions/set_lesson_request.sql
--!include functions/set_event_registration.sql
--!include functions/event_trainers.sql
--!include functions/event_remaining_x.sql
--!include functions/event_series_info.sql
--!include functions/event_share_token.sql
--!include functions/event_sharing.sql
--!include functions/tg_event__validate_parent.sql
--!include functions/tg_event__delete_payment_on_cancellation.sql
--!include functions/tg_transaction__effective_date.sql
--!include functions/create_latest_lesson_payments.sql
--!include functions/system_admin_tenants.sql
--!include functions/event_stats.sql
--!include functions/tg_event_registration__attendance_timestamps.sql
--!include functions/create_event_payment.sql
--!include functions/tg_event_registration__sync_lesson_payment.sql
--!include functions/event_registration_last_attended.sql
--!include functions/scoreboard.sql
--!include functions/event_share_claims.sql

--!include views/event_people.sql
--!include functions/accessible_event_ids.sql
--!include functions/tg_event_people__refresh.sql
--!include functions/visible_person_ids.sql

--!include policies/event.sql
--!include policies/event_series.sql
--!include policies/event_trainer.sql
--!include policies/event_target_cohort.sql
--!include policies/cohort.sql
--!include policies/event_registration.sql
--!include policies/event_lesson_request.sql
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

-- Drop the old permission helper after its policies have been replaced.
drop function if exists app_private.can_trainer_edit_instance;

alter table otp_token
  add column if not exists intent text not null default 'password_reset',
  add column if not exists event_id bigint references event(id) on delete cascade;

alter table otp_token drop constraint if exists otp_token_intent_check;
alter table otp_token add constraint otp_token_intent_check check (
  (intent = 'password_reset' and event_id is null)
  or (intent = 'event_handoff' and event_id is not null)
);

--!include functions/otp_login.sql

do $$
begin
  if to_regclass('public.event_external_registration') is not null then
    lock table event_external_registration in access exclusive mode;
    if exists (select 1 from event_external_registration) then
      raise exception 'Convert external registrations with the manual script before removing the table';
    end if;
  end if;
end;
$$;

--!include functions/event_registration_info.sql

drop table if exists event_external_registration;

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
--!include functions/update_attendance.sql
--!include functions/federation_people_facets.sql
--!include functions/federation_competitor_facets.sql
--!include functions/federation_event_facets.sql
--!include views/competition_participation.sql

comment on table federated.age_group is '@name age_group
@behavior -insert -update -delete +list -connection';
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
comment on type federated.competitor_type is '@name competitor_type';
comment on type federated.competition_type is '@name competition_type';
comment on type federated.gender is '@name gender';
comment on type federated.person_license_discipline is '@name person_license_discipline';
comment on type federated.person_license_kind is '@name person_license_kind';
comment on type federated.person_license_status is '@name person_license_status';
comment on type federated.official_role is '@name official_role';
comment on type federated.score_component is '@name score_component';
comment on type federated.scoring_method is '@name scoring_method';

comment on column federated.event_official.role is '@behavior +attribute:orderBy';
comment on column federated.competition_official.role is '@behavior +attribute:orderBy';
comment on column federated.person_license.kind is '@behavior +attribute:orderBy';
comment on column federated.person_license.discipline is '@behavior +attribute:orderBy';
comment on column federated.judge_score.component is '@behavior +attribute:orderBy';
comment on column federated.competitor_component.role is '@behavior +attribute:orderBy';

comment on type event_registration_status is null;
comment on function set_event_sharing is null;
comment on function update_tenant_settings_key is null;
comment on function confirm_membership_application is null;
comment on function update_attendance is null;
comment on function upsert_announcement is null;
comment on function upsert_article is null;
comment on function upsert_location is null;
comment on function create_person is null;
comment on function create_credit_transaction_for_person is null;

comment on column event_registration.event_id is null;
comment on column payment.event_id is null;

comment on column event_trainer.event_id is null;
comment on column event_trainer.lessons_offered is null;
comment on column event.series_id is null;
comment on column event_target_cohort.event_id is null;
comment on column app_private.system_admin_user.user_id is null;
comment on column app_private.system_admin_user.created_at is null;
comment on column app_private.system_admin_user.created_by is null;
comment on column federated.competition.competition_type is null;

--!include views/person_last_activity.sql

comment on table event_target_cohort is '@omit create,update,delete
@behavior -query:resource:list -query:resource:connection -query:resource:single
@simpleCollections only';

comment on constraint event_target_cohort_event_id_fkey on event_target_cohort is '@fieldName event
@foreignFieldName targetCohorts';

comment on table event_trainer is '@omit create,update,delete
@behavior -query:resource:list -query:resource:connection -query:resource:single
@simpleCollections only';

comment on constraint event_trainer_tenant_id_event_id_fkey on event_trainer is '@fieldName event
@foreignFieldName eventTrainers';

--!include functions/event_trainer_name.sql
--!include functions/event_trainer_tenant_trainer.sql

drop view if exists tenant_location_image;
drop view if exists tenant_location;

comment on table access_event is '@omit create,update,delete
@simpleCollections only';

comment on table event is '@omit create
@simpleCollections only';

comment on constraint access_event_location_fkey on access_event is '@fieldName location
@foreignFieldName accessEvents';

comment on constraint event_location_fkey on event is '@fieldName location
@foreignFieldName events';

comment on table event_lesson_request is '@omit create,update,delete
@behavior -query:resource:list -query:resource:connection -query:resource:single
@simpleCollections only';

comment on constraint event_lesson_request_registration_id_fkey on event_lesson_request is '@foreignFieldName lessonRequests';
comment on constraint event_lesson_request_trainer_id_fkey on event_lesson_request is '@foreignFieldName lessonRequests';
comment on constraint event_lesson_request_tenant_id_fkey on event_lesson_request is '@foreignFieldName lessonRequests';

comment on table event_registration is '@omit create,update,delete
@simpleCollections both';

comment on constraint event_registration_event_id_fkey on event_registration is '@fieldName event
@foreignFieldName registrations';

comment on constraint event_registration_parent_registration_id_fkey on event_registration is '@fieldName parentRegistration
@foreignFieldName childRegistrations';

comment on constraint event_parent_id_fkey on event is '@fieldName parent
@foreignFieldName childEvents';

comment on constraint payment_event_id_fkey on payment is '@fieldName event
@foreignFieldName payments';

--!include functions/activity_timeline.sql
--!include functions/event_overlaps_reports.sql
--!include functions/event_approx_price.sql

drop function if exists event_instance_facets;
drop function if exists move_event_instance;

--!include functions/event_instances_for_range.sql
--!include functions/event_facets.sql
--!include functions/move_event.sql
--!include functions/trainer_group_attendance_completion.sql
