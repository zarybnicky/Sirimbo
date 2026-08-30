drop function if exists upsert_announcement;
drop type if exists announcement_type_input;
drop type if exists announcement_audience_type_input;

create type announcement_type_input as (
  id bigint,
  title text,
  body text,
  is_sticky boolean,
  scheduled_since timestamptz,
  scheduled_until timestamptz,
  status announcement_status
);

create type announcement_audience_type_input as (
  id bigint,
  cohort_id bigint,
  audience_role announcement_audience_role
);

create or replace function upsert_announcement(
  info announcement_type_input,
  audiences announcement_audience_type_input[] default null,
  attachments bigint[] default null
)
  returns announcement language plpgsql
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
    select * into result from announcement where id = info.id;
    if not found then
      raise exception 'Announcement with id % not found', info.id;
    end if;
  end if;

  if attachments is not null then
    select coalesce(array_agg(id), '{}'::bigint[])
    into attachments
    from file
    where id = any(attachments)
      and tenant_id = result.tenant_id
      and uploaded_at is not null;

    delete from announcement_attachment
    where announcement_id = result.id
      and not inline
      and file_id <> all(attachments);

    insert into announcement_attachment (tenant_id, announcement_id, file_id, inline)
    select result.tenant_id, result.id, file_id, false
    from unnest(attachments) input(file_id)
    on conflict (tenant_id, announcement_id, file_id)
    do update set inline = false;
  end if;

  if info.id is not null then
    -- Make sure the update trigger re-populates inline references
    update announcement set
      title = info.title,
      body = info.body,
      status = coalesce(info.status, status),
      is_sticky = coalesce(info.is_sticky, false),
      scheduled_since = info.scheduled_since,
      scheduled_until = info.scheduled_until
    where id = info.id
    returning * into result;
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
