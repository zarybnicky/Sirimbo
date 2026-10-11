-- Run with psql -X -v ON_ERROR_STOP=1 -f migrations/manual/2026-09-30-backfill-group-registration-cohorts.sql
-- as a role that can see and update registrations across tenants.
-- Review the counts and sample rows. Change the final ROLLBACK to COMMIT only
-- after checking them against production. Adjust the date window if the old
-- save_events implementation remained deployed after 2026-09-30.

begin;
set local row_security = off;

create temp table backfill_group_cohort_candidates on commit drop as
with affected as materialized (
  select registration.id, registration.tenant_id, registration.instance_id,
    registration.person_id, instance.since, registration.updated_at
  from public.event_instance_registration registration
  join public.event_instance instance
    on instance.id = registration.instance_id
    and instance.tenant_id = registration.tenant_id
  where instance.type = 'group'
    and registration.parent_registration_id is null
    and registration.person_id is not null
    and registration.source = 'manager'
    and registration.registration_status = 'active'
    and registration.target_cohort_id is null
    and registration.updated_at >= timestamptz '2026-08-13 00:00:00 Europe/Prague'
    and registration.updated_at < timestamptz '2026-10-01 00:00:00 Europe/Prague'
    and exists (
      select 1 from public.event_instance_target_cohort target
      where target.instance_id = registration.instance_id
        and target.tenant_id = registration.tenant_id
    )
), memberships as (
  select distinct affected.id, target.cohort_id
  from affected
  join public.event_instance_target_cohort target
    on target.instance_id = affected.instance_id
    and target.tenant_id = affected.tenant_id
  join public.cohort_membership membership
    on membership.cohort_id = target.cohort_id
    and membership.tenant_id = affected.tenant_id
    and membership.person_id = affected.person_id
    and (affected.since < now() or (
      membership.status = 'active' and membership.active_range @> now()
    ))
), choices as (
  select affected.id, affected.tenant_id, affected.instance_id,
    affected.person_id, affected.updated_at,
    count(memberships.cohort_id) as membership_matches,
    min(memberships.cohort_id) as sole_membership_cohort_id
  from affected
  left join memberships on memberships.id = affected.id
  group by affected.id, affected.tenant_id, affected.instance_id,
    affected.person_id, affected.updated_at
)
select id, tenant_id, instance_id, person_id, updated_at,
  membership_matches,
  case
    when membership_matches = 1 then sole_membership_cohort_id
  end as cohort_id,
  case
    when membership_matches = 1 then 'sole recorded membership'
    when membership_matches = 0 then 'no recorded membership'
    else 'ambiguous memberships'
  end as reason
from choices;

select reason, count(*) as registrations, count(distinct instance_id) as events
from backfill_group_cohort_candidates
group by reason order by reason;

select * from backfill_group_cohort_candidates
where cohort_id is null
order by tenant_id, instance_id, person_id
limit 100;

select * from backfill_group_cohort_candidates
where cohort_id is not null
order by tenant_id, instance_id, person_id
limit 100;

create temp table backfill_group_cohort_changed on commit drop as
with changed as (
  update public.event_instance_registration registration
  set source = 'cohort', target_cohort_id = candidate.cohort_id
  from backfill_group_cohort_candidates candidate
  where registration.id = candidate.id
    and registration.tenant_id = candidate.tenant_id
    and candidate.cohort_id is not null
    and registration.source = 'manager'
    and registration.target_cohort_id is null
    and registration.registration_status = 'active'
  returning registration.id, registration.tenant_id, registration.instance_id,
    registration.person_id, registration.target_cohort_id
)
select * from changed;

select count(*) as changed_registrations,
  count(distinct instance_id) as changed_events
from backfill_group_cohort_changed;

do $$
begin
  if (select count(*) from backfill_group_cohort_changed) <>
     (select count(*) from backfill_group_cohort_candidates where cohort_id is not null) then
    raise exception 'Not every candidate was updated; transaction must roll back';
  end if;
end;
$$;

rollback;
