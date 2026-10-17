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

grant all on table aktuality to anonymous;

--!include functions/upsert_article.sql

drop materialized view if exists app_private.event_people;
--!include views/event_people.sql
--!include functions/attended_event_ids.sql
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
