create or replace function app_private.refresh_event_instance_stats(p_instance_id bigint)
  returns void
  language sql volatile security definer
  set search_path = pg_catalog, pg_temp
as $$
  select 1 from public.event_instance where id = p_instance_id for no key update;

  update public.event_instance instance
  set stats = actual.stats
  from (
    select jsonb_build_object(
      'TOTAL', count(*)::int,
      'UNKNOWN', count(*) filter (where status = 'unknown')::int,
      'ATTENDED', count(*) filter (where status = 'attended')::int,
      'NOT_EXCUSED', count(*) filter (where status = 'not-excused')::int
    ) as stats
    from public.event_instance_registration
    where instance_id = p_instance_id
      and person_id is not null
      and registration_status = 'active'
  ) actual
  where instance.id = p_instance_id and instance.stats is distinct from actual.stats;
$$;

create or replace function app_private.tg_eir__refresh_stats_stmt()
  returns trigger
  language plpgsql security definer
  set search_path = pg_catalog, pg_temp
as $$
-- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
begin
  if tg_op = 'INSERT' then
    perform app_private.refresh_event_instance_stats(affected.instance_id)
    from (
      select distinct instance_id from changed_rows
      where person_id is not null order by instance_id
    ) affected;
  elsif tg_op = 'DELETE' then
    perform app_private.refresh_event_instance_stats(affected.instance_id)
    from (
      select distinct instance_id from deleted_rows
      where person_id is not null order by instance_id
    ) affected;
  else
    perform app_private.refresh_event_instance_stats(affected.instance_id)
    from (
      select distinct affected.instance_id
      from deleted_rows old_row
      join changed_rows new_row using (id)
      cross join lateral (values
        (old_row.instance_id, old_row.person_id),
        (new_row.instance_id, new_row.person_id)
      ) affected(instance_id, person_id)
      where row(old_row.instance_id, old_row.person_id, old_row.status, old_row.registration_status)
        is distinct from
        row(new_row.instance_id, new_row.person_id, new_row.status, new_row.registration_status)
        and affected.person_id is not null
      order by affected.instance_id
    ) affected;
  end if;
  return null;
end;
$$;
