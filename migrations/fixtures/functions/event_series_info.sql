create or replace function app_private.refresh_event_series_info(p_series_ids bigint[])
  returns void
  language plpgsql security definer
  set search_path = pg_catalog, public, pg_temp
as $$
begin
  if coalesce(cardinality(p_series_ids), 0) = 0 then
    return;
  end if;

  perform id from event_series
  where id = any(p_series_ids) order by id for no key update;

  with actual as (
    select member.id,
      row(series.id, series.name,
        row_number() over ordered::integer,
        count(*) over whole_series::integer,
        min(member.since) over whole_series,
        max(member.until) over whole_series)::series_info as info
    from event member
    join event_series series
      on series.tenant_id = member.tenant_id and series.id = member.series_id
    where series.id = any(p_series_ids)
    window whole_series as (partition by member.tenant_id, member.series_id),
      ordered as (whole_series order by member.since, member.id)
  )
  update event member set series_info = actual.info
  from actual
  where member.id = actual.id and member.series_info is distinct from actual.info;
end;
$$;

create or replace function app_private.tg_event__refresh_series_info()
  returns trigger
  language plpgsql security definer
  set search_path = pg_catalog, public, pg_temp
as $$
-- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
declare
  series_ids bigint[];
begin
  if tg_op = 'INSERT' then
    select array_agg(distinct series_id) into series_ids
    from changed_rows where series_id is not null;
  elsif tg_op = 'DELETE' then
    select array_agg(distinct series_id) into series_ids
    from deleted_rows where series_id is not null;
  else
    select array_agg(distinct affected.series_id) into series_ids
    from deleted_rows old_row full join changed_rows new_row using (id)
    cross join lateral (values (old_row.series_id), (new_row.series_id)) affected(series_id)
    where row(old_row.id, old_row.tenant_id, old_row.series_id, old_row.since, old_row.until)
      is distinct from
      row(new_row.id, new_row.tenant_id, new_row.series_id, new_row.since, new_row.until)
      and affected.series_id is not null;
  end if;

  if tg_op <> 'DELETE' then
    if exists (select 1 from changed_rows
      where series_id is null and series_info is distinct from null) then
      update event member set series_info = null
      from changed_rows changed
      where member.id = changed.id and changed.series_id is null
        and member.series_info is distinct from null;
    end if;
  end if;

  perform app_private.refresh_event_series_info(series_ids);
  return null;
end;
$$;

create or replace function app_private.tg_event_series__refresh_info()
  returns trigger
  language plpgsql security definer
  set search_path = pg_catalog, public, pg_temp
as $$
-- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
declare
  series_ids bigint[];
begin
  select array_agg(changed.id) into series_ids
  from changed_rows changed join deleted_rows previous using (id)
  where changed.name is distinct from previous.name;
  perform app_private.refresh_event_series_info(series_ids);
  return null;
end;
$$;

create or replace trigger _600_refresh_series_info_ins
  after insert on public.event
  referencing new table as changed_rows for each statement
  execute function app_private.tg_event__refresh_series_info();
create or replace trigger _600_refresh_series_info_del
  after delete on public.event
  referencing old table as deleted_rows for each statement
  execute function app_private.tg_event__refresh_series_info();
create or replace trigger _600_refresh_series_info_upd
  after update on public.event
  referencing old table as deleted_rows new table as changed_rows for each statement
  execute function app_private.tg_event__refresh_series_info();
create or replace trigger _600_refresh_series_info_name
  after update on public.event_series
  referencing old table as deleted_rows new table as changed_rows for each statement
  execute function app_private.tg_event_series__refresh_info();
