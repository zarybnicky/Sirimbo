create function event_instance_share_token(i event_instance)
  returns text
  language sql
  stable
as $$
  select share_token
  from event_instance
  where id = $1.id
    and (pg_has_role(current_user, 'administrator', 'member')
      or (pg_has_role(current_user, 'trainer', 'member')
      and app_private.can_trainer_edit_instance($1.id)))
$$;
grant execute on function event_instance_share_token to anonymous;

create function set_event_sharing(id bigint, p_enabled boolean)
  returns text
  language plpgsql
  volatile
as $$
declare
  v_share_token text;
begin
  update event_instance instance
  set share_token = case
    when not coalesce(p_enabled, false) then null
    when instance.share_token is null
      then translate(encode(gen_random_bytes(24), 'base64'), '+/', '-_')
    else instance.share_token
  end
  where instance.id = $1
  returning instance.share_token into v_share_token;

  if not found then
    raise exception 'INSTANCE_NOT_FOUND' using errcode = '22023';
  end if;

  return v_share_token;
end;
$$;

grant execute on function set_event_sharing to anonymous;

create or replace function app_private.event_share_claims(p_tenant_id bigint, p_share text)
returns table (
  instance_ids bigint[],
  person_ids bigint[],
  couple_ids bigint[],
  cohort_ids bigint[]
)
language sql
stable
security definer
set search_path = pg_catalog, public, pg_temp
as $$
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
$$;

revoke all on function app_private.event_share_claims from public;
