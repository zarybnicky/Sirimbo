drop trigger if exists _500_eir_refresh_stats_ins on event_registration;
drop trigger if exists _500_eir_refresh_stats_del on event_registration;
drop trigger if exists _500_eir_refresh_stats_upd on event_registration;
drop function if exists app_private.tg_eir__refresh_stats_stmt;
drop function if exists app_private.refresh_event_instance_stats;

create or replace function app_private.refresh_event_stats(p_event_id bigint)
  returns void
  language sql volatile security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select 1 from event where id = p_event_id for no key update;

  update event instance
  set stats = actual.stats
  from (
    select jsonb_build_object(
      'TOTAL', count(*)::int,
      'UNKNOWN', count(*) filter (where status = 'unknown')::int,
      'ATTENDED', count(*) filter (where status = 'attended')::int,
      'NOT_EXCUSED', count(*) filter (where status = 'not-excused')::int
    ) as stats
    from event_registration
    where event_id = p_event_id
      and person_id is not null
      and registration_status = 'active'
  ) actual
  where instance.id = p_event_id and instance.stats is distinct from actual.stats;
$$;

create or replace function app_private.tg_event_registration__refresh_stats_stmt()
  returns trigger
  language plpgsql security definer
  set search_path = pg_catalog, pg_temp
as $$
-- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
begin
  if tg_op = 'INSERT' then
    perform app_private.refresh_event_stats(affected.event_id)
    from (
      select distinct event_id from changed_rows
      where person_id is not null order by event_id
    ) affected;
  elsif tg_op = 'DELETE' then
    perform app_private.refresh_event_stats(affected.event_id)
    from (
      select distinct event_id from deleted_rows
      where person_id is not null order by event_id
    ) affected;
  else
    perform app_private.refresh_event_stats(affected.event_id)
    from (
      select distinct affected.event_id
      from deleted_rows old_row
      join changed_rows new_row using (id)
      cross join lateral (values
        (old_row.event_id, old_row.person_id),
        (new_row.event_id, new_row.person_id)
      ) affected(event_id, person_id)
      where row(old_row.event_id, old_row.person_id, old_row.status, old_row.registration_status)
        is distinct from
        row(new_row.event_id, new_row.person_id, new_row.status, new_row.registration_status)
        and affected.person_id is not null
      order by affected.event_id
    ) affected;
  end if;
  return null;
end;
$$;

revoke execute on function app_private.tg_event_registration__refresh_stats_stmt() from public, anonymous;

create or replace trigger _500_refresh_stats_ins
  after insert on event_registration
  referencing new table as changed_rows for each statement
  execute function app_private.tg_event_registration__refresh_stats_stmt();
create or replace trigger _500_refresh_stats_del
  after delete on event_registration
  referencing old table as deleted_rows for each statement
  execute function app_private.tg_event_registration__refresh_stats_stmt();
create or replace trigger _500_refresh_stats_upd
  after update on event_registration
  referencing old table as deleted_rows new table as changed_rows for each statement
  execute function app_private.tg_event_registration__refresh_stats_stmt();
