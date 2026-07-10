CREATE FUNCTION app_private.tg_eir__attendance_timestamps() RETURNS trigger
    LANGUAGE plpgsql
    SET search_path TO 'pg_catalog', 'pg_temp'
    AS $$
begin
  if new.person_id is null then
    new.attendance_created_at := null;
    new.attendance_updated_at := null;
  elsif tg_op = 'INSERT' then
    new.attendance_created_at := new.created_at;
    new.attendance_updated_at := new.updated_at;
  elsif old.person_id is null or new.person_id is distinct from old.person_id then
    new.attendance_created_at := new.created_at;
    new.attendance_updated_at := new.updated_at;
  else
    new.attendance_created_at := old.attendance_created_at;
    if new.status is distinct from old.status or new.note is distinct from old.note then
      new.attendance_updated_at := case
        when old.attendance_updated_at >= new.updated_at
          then old.attendance_updated_at + interval '1 millisecond'
        else new.updated_at
      end;
    else
      new.attendance_updated_at := old.attendance_updated_at;
    end if;
  end if;
  return new;
end;
$$;

REVOKE ALL ON FUNCTION app_private.tg_eir__attendance_timestamps() FROM PUBLIC;
