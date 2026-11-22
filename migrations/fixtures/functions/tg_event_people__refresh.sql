create or replace function app_private.tg_event_people__refresh() returns trigger
language plpgsql security definer
set search_path = pg_catalog, pg_temp
as $$
-- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
declare
  has_changes boolean;
  target_tenant_id bigint;
  target_person_id bigint;
begin
  if tg_op = 'INSERT' then
    has_changes := exists (select 1 from changed_rows);
  elsif tg_op = 'DELETE' then
    has_changes := exists (select 1 from deleted_rows);
  else
    -- The trigger argument supplies the view's input columns for each table.
    -- Compare sets: row order and duplicate inputs do not affect event_people.
    execute format(
      'select exists (
        (select %1$s from changed_rows except select %1$s from deleted_rows)
        union all
        (select %1$s from deleted_rows except select %1$s from changed_rows)
      )', tg_argv[0]
    ) into has_changes;
  end if;

  if has_changes then
    -- Refresh tenant visibility immediately; the viewer/person matview stays queued.
    if tg_table_name in ('event_registration', 'event_trainer') then
      for target_tenant_id, target_person_id in execute case tg_op
        when 'INSERT' then 'select distinct tenant_id, person_id from changed_rows where person_id is not null order by tenant_id, person_id'
        when 'DELETE' then 'select distinct tenant_id, person_id from deleted_rows where person_id is not null order by tenant_id, person_id'
        else format(
          'select distinct affected.tenant_id, affected.person_id
           from deleted_rows old_row full join changed_rows new_row using (id)
           cross join lateral (values (old_row.tenant_id, old_row.person_id),
                                      (new_row.tenant_id, new_row.person_id)) affected(tenant_id, person_id)
           where affected.person_id is not null and (
             (old_row.tenant_id, old_row.person_id) is distinct from (new_row.tenant_id, new_row.person_id)
             or %s)
           order by affected.tenant_id, affected.person_id',
          case when tg_table_name = 'event_registration'
            then 'old_row.registration_status is distinct from new_row.registration_status'
            else 'false' end
        )
      end loop
        perform app_private.rebuild_tenant_relation(target_tenant_id, target_person_id);
      end loop;
    end if;

    perform graphile_worker.add_job(
      'refresh_event_people',
      queue_name := 'refresh_event_people',
      job_key := 'refresh_event_people'
    );
  end if;
  return null;
end;
$$;

drop trigger if exists _900_refresh_event_people on event_registration;

create or replace trigger _900_refresh_event_people_ins
  after insert on event_registration
  referencing new table as changed_rows for each statement
  execute function app_private.tg_event_people__refresh();
create or replace trigger _900_refresh_event_people_del
  after delete on event_registration
  referencing old table as deleted_rows for each statement
  execute function app_private.tg_event_people__refresh();
create or replace trigger _900_refresh_event_people_upd
  after update on event_registration
  referencing old table as deleted_rows new table as changed_rows for each statement
  execute function app_private.tg_event_people__refresh('tenant_id, event_id, person_id, registration_status');

drop trigger if exists _900_refresh_event_people on event_trainer;

create or replace trigger _900_refresh_event_people_ins
  after insert on event_trainer
  referencing new table as changed_rows for each statement
  execute function app_private.tg_event_people__refresh();
create or replace trigger _900_refresh_event_people_del
  after delete on event_trainer
  referencing old table as deleted_rows for each statement
  execute function app_private.tg_event_people__refresh();
create or replace trigger _900_refresh_event_people_upd
  after update on event_trainer
  referencing old table as deleted_rows new table as changed_rows for each statement
  execute function app_private.tg_event_people__refresh('tenant_id, event_id, person_id');

drop trigger if exists _900_refresh_event_people on event;

create or replace trigger _900_refresh_event_people_ins
  after insert on event
  referencing new table as changed_rows for each statement
  execute function app_private.tg_event_people__refresh();
create or replace trigger _900_refresh_event_people_del
  after delete on event
  referencing old table as deleted_rows for each statement
  execute function app_private.tg_event_people__refresh();
create or replace trigger _900_refresh_event_people_upd
  after update on event
  referencing old table as deleted_rows new table as changed_rows for each statement
  execute function app_private.tg_event_people__refresh('tenant_id, id, parent_id');
