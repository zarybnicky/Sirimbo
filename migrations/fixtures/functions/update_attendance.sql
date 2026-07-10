create or replace function update_attendance(eir_id bigint, status attendance_type, note text)
  returns event_instance_registration
  language sql as $$
  update event_instance_registration eir
  set status = $2, attendance_note = $3
  where eir.id = $1 and eir.person_id is not null and eir.registration_status = 'active'
  returning eir.*;
$$;
