alter table file add column if not exists url text
  generated always as ('/f/' || id || '/' || name) stored not null;

create or replace function app_private.tg__timestamps() returns trigger language plpgsql as $$
declare
  ignored text[];
begin
  if tg_op = 'UPDATE' then
    -- Don't bump updated_at for no-op updates
    if new is not distinct from old then
      return new;
    end if;

    select coalesce(array_agg(attribute.attname::text order by attribute.attnum), '{}') || tg_argv
      into ignored
    from pg_catalog.pg_attribute as attribute
    where attribute.attrelid = tg_relid
      and attribute.attnum > 0
      and not attribute.attisdropped
      and attribute.attgenerated <> '';

    -- Ignore generated columns and any independent state named by the trigger
    if ignored <> '{}' and to_jsonb(new) - ignored is not distinct from to_jsonb(old) - ignored then
      return new;
    end if;
  end if;

  new.created_at = case when tg_op = 'INSERT' then now() else old.created_at end;
  new.updated_at = now();
  return new;
end;
$$;

create or replace trigger _100_timestamps
  before insert or update on event_instance_registration
  for each row execute function app_private.tg__timestamps(
    'attendance_created_at', 'attendance_updated_at'
  );

create or replace trigger _100_timestamps
  before insert or update on event_instance
  for each row execute function app_private.tg__timestamps('manager_person_ids', 'stats');

create or replace trigger _100_timestamps
  before insert or update on users
  for each row execute function app_private.tg__timestamps(
    'last_login', 'last_active_at', 'last_version'
  );

create or replace trigger _100_timestamps
  before insert or update on event_instance_trainer
  for each row execute function app_private.tg__timestamps();

create or replace trigger _100_timestamps
  before insert or update on event_lesson_demand
  for each row execute function app_private.tg__timestamps();

create or replace trigger _100_timestamps
  before insert or update on otp_token
  for each row execute function app_private.tg__timestamps();

create or replace trigger _100_timestamps
  before insert or update on person_invitation
  for each row execute function app_private.tg__timestamps();

create or replace trigger _100_timestamps
  before insert or update on tenant_location
  for each row execute function app_private.tg__timestamps();

--! include functions/competition_reports.sql

comment on table aktuality is '@omit create,update
@behavior -query:resource:list';
