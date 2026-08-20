create or replace function upsert_announcement(
  info announcement_type_input,
  audiences announcement_audience_type_input[] default null
) returns announcement
language plpgsql
as $$
declare
  result announcement;
begin
  if info.id is null then
    insert into announcement (
      title, body, status, is_sticky, scheduled_since, scheduled_until
    ) values (
      info.title,
      info.body,
      coalesce(info.status, 'draft'),
      coalesce(info.is_sticky, false),
      info.scheduled_since,
      info.scheduled_until
    ) returning * into result;
  else
    update announcement set
      title = info.title,
      body = info.body,
      status = coalesce(info.status, status),
      is_sticky = coalesce(info.is_sticky, false),
      scheduled_since = info.scheduled_since,
      scheduled_until = info.scheduled_until
    where id = info.id
    returning * into result;

    if not found then
      raise exception 'Announcement with id % not found', info.id;
    end if;
  end if;

  if audiences is not null then
    with input as (
      select distinct (item).id, (item).cohort_id, (item).audience_role
      from unnest(audiences) item
    )
    delete from announcement_audience audience
    using input
    where audience.announcement_id = result.id
      and audience.id = input.id
      and input.id is not null
      and input.cohort_id is null
      and input.audience_role is null;

    with input as (
      select distinct (item).id, (item).cohort_id, (item).audience_role
      from unnest(audiences) item
    )
    update announcement_audience audience
    set cohort_id = input.cohort_id,
        audience_role = input.audience_role
    from input
    where audience.announcement_id = result.id
      and audience.id = input.id
      and input.id is not null
      and ((input.cohort_id is not null) <> (input.audience_role is not null))
      and (audience.cohort_id, audience.audience_role)
        is distinct from (input.cohort_id, input.audience_role);

    with input as (
      select distinct (item).cohort_id
      from unnest(audiences) item
      where (item).id is null
        and (item).cohort_id is not null
        and (item).audience_role is null
    )
    insert into announcement_audience (announcement_id, cohort_id)
    select result.id, input.cohort_id
    from input
    on conflict (announcement_id, cohort_id) do nothing;

    with input as (
      select distinct (item).audience_role
      from unnest(audiences) item
      where (item).id is null
        and (item).cohort_id is null
        and (item).audience_role is not null
    )
    insert into announcement_audience (announcement_id, audience_role)
    select result.id, input.audience_role
    from input
    on conflict (announcement_id, audience_role) do nothing;
  end if;

  return result;
end;
$$;

grant all on function upsert_announcement to anonymous;
