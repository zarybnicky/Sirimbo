CREATE FUNCTION public.upsert_announcement(info public.announcement_type_input, cohorts public.announcement_cohort_type_input[] DEFAULT NULL::public.announcement_cohort_type_input[], audiences public.announcement_audience_type_input[] DEFAULT NULL::public.announcement_audience_type_input[]) RETURNS public.announcement
    LANGUAGE plpgsql
    AS $$
declare
  v_announcement announcement;
begin
  if info.id is not null then
    update announcement set
      title = info.title,
      body = info.body,
      is_locked = coalesce(info.is_locked, false),
      is_visible = coalesce(info.is_visible, true),
      is_sticky = coalesce(info.is_sticky, false),
      scheduled_since = info.scheduled_since,
      scheduled_until = info.scheduled_until
    where id = info.id
    returning * into v_announcement;

    if not found then
      raise exception 'Announcement with id % not found', info.id;
    end if;
  else
    insert into announcement (
      title,
      body,
      is_locked,
      is_visible,
      is_sticky,
      scheduled_since,
      scheduled_until
    )
    values (
      info.title,
      info.body,
      coalesce(info.is_locked, false),
      coalesce(info.is_visible, true),
      coalesce(info.is_sticky, false),
      info.scheduled_since,
      info.scheduled_until
    )
    returning * into v_announcement;
  end if;

  if cohorts is not null then
    with cohort_input as (
      select distinct (c).id as id, (c).cohort_id as cohort_id
      from unnest(cohorts) c
    )
    delete from announcement_audience aa
    using cohort_input ci
    where aa.announcement_id = v_announcement.id
      and aa.id = ci.id
      and ci.id is not null
      and ci.cohort_id is null;

    with cohort_input as (
      select distinct (c).id as id, (c).cohort_id as cohort_id
      from unnest(cohorts) c
    )
    update announcement_audience aa
    set cohort_id = ci.cohort_id
    from cohort_input ci
    where aa.announcement_id = v_announcement.id
      and aa.id = ci.id
      and ci.id is not null
      and ci.cohort_id is not null
      and aa.cohort_id is distinct from ci.cohort_id;

    with cohort_input as (
      select distinct (c).cohort_id as cohort_id
      from unnest(cohorts) c
      where (c).id is null and (c).cohort_id is not null
    )
    insert into announcement_audience (announcement_id, cohort_id)
    select v_announcement.id, ci.cohort_id
    from cohort_input ci
    on conflict (announcement_id, cohort_id) do nothing;
  end if;

  if audiences is not null then
    with role_input as (
      select distinct (a).id as id, (a).audience_role as audience_role
      from unnest(audiences) a
    )
    delete from announcement_audience aa
    using role_input ri
    where aa.announcement_id = v_announcement.id
      and aa.id = ri.id
      and ri.id is not null
      and ri.audience_role is null;

    with role_input as (
      select distinct (a).id as id, (a).audience_role as audience_role
      from unnest(audiences) a
    )
    update announcement_audience aa
    set audience_role = ri.audience_role
    from role_input ri
    where aa.announcement_id = v_announcement.id
      and aa.id = ri.id
      and ri.id is not null
      and ri.audience_role is not null
      and aa.audience_role is distinct from ri.audience_role;

    with role_input as (
      select distinct (a).audience_role as audience_role
      from unnest(audiences) a
      where (a).id is null and (a).audience_role is not null
    )
    insert into announcement_audience (announcement_id, audience_role)
    select v_announcement.id, ri.audience_role
    from role_input ri
    on conflict (announcement_id, audience_role) do nothing;
  end if;

  return v_announcement;
end;
$$;

GRANT ALL ON FUNCTION public.upsert_announcement(info public.announcement_type_input, cohorts public.announcement_cohort_type_input[], audiences public.announcement_audience_type_input[]) TO anonymous;
