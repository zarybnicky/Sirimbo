CREATE FUNCTION app_private.event_share_claims(p_tenant_id bigint, p_share text) RETURNS TABLE(instance_ids bigint[], person_ids bigint[], couple_ids bigint[], cohort_ids bigint[])
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $_$
  with root as (
    select id
    from event_instance
    where tenant_id = p_tenant_id
      and (
        share_token = p_share
        or (
          is_public
          and has_public_details
          and id = case
            when p_share ~ '^\d{1,18}$' then p_share::bigint
          end
        )
      )
  ),
  events as (
    select id from root
    union all
    select id from event_instance where parent_id in (select id from root)
  ),
  trainers as (
    select person_id
    from event_instance_trainer
    where instance_id in (select id from events)
  ),
  registrations as (
    select person_id, couple_id
    from event_instance_registration
    where instance_id in (select id from events)
      and parent_registration_id is null
      and registration_status = 'active'
  ),
  targets as (
    select cohort_id
    from event_instance_target_cohort
    where instance_id in (select id from events)
  )
  select
    array(select id from events),
    array(
      select person_id from trainers
      union
      select person_id from registrations where person_id is not null
      union
      select couple.man_id
      from registrations join couple on couple.id = registrations.couple_id
      union
      select couple.woman_id
      from registrations join couple on couple.id = registrations.couple_id
    ),
    array(
      select distinct couple_id
      from registrations
      where couple_id is not null
    ),
    array(select distinct cohort_id from targets)
  where exists (select 1 from root);
$_$;

REVOKE ALL ON FUNCTION app_private.event_share_claims(p_tenant_id bigint, p_share text) FROM PUBLIC;
