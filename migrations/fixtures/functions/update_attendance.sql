create or replace function update_attendance(eir_id bigint, status attendance_type, note text)
  returns event_instance_registration
  language sql
as $$
  update event_instance_registration set status = $2, attendance_note = $3 where id = $1 returning *;
$$;
