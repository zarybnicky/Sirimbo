create or replace function app_private.tg_event_people__refresh() returns trigger
language plpgsql security definer
set search_path = pg_catalog, pg_temp
as $$
-- @plpgsql_check_options: oldtable = deleted_rows, newtable = changed_rows
declare
  has_changes boolean;
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
  execute function app_private.tg_event_people__refresh('event_id, person_id, registration_status');

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
  execute function app_private.tg_event_people__refresh('event_id, person_id');

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
  execute function app_private.tg_event_people__refresh('id, parent_id');
