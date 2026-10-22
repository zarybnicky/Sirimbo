drop trigger if exists _200_eir_attendance_timestamps on event_registration;
drop function if exists app_private.tg_eir__attendance_timestamps;

create or replace function app_private.tg_event_registration__attendance_timestamps()
  returns trigger
  language plpgsql
  set search_path = pg_catalog, pg_temp
as $$
begin
  if new.person_id is null then
    new.attendance_created_at := null;
    new.attendance_updated_at := null;
  elsif tg_op = 'INSERT' or new.person_id is distinct from old.person_id then
    new.attendance_created_at := new.created_at;
    new.attendance_updated_at := new.updated_at;
  else
    new.attendance_created_at := old.attendance_created_at;
    new.attendance_updated_at := case
      when new.status is not distinct from old.status
        and new.attendance_note is not distinct from old.attendance_note
        then old.attendance_updated_at
      when old.attendance_updated_at >= new.updated_at
        then old.attendance_updated_at + interval '1 millisecond'
      else new.updated_at
    end;
  end if;
  return new;
end;
$$;
revoke execute on function app_private.tg_event_registration__attendance_timestamps() from public, anonymous;

create or replace trigger _200_attendance_timestamps
  before insert or update of person_id, status, attendance_note
  on event_registration
  for each row execute function app_private.tg_event_registration__attendance_timestamps();
