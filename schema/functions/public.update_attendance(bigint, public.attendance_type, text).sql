CREATE FUNCTION public.update_attendance(eir_id bigint, status public.attendance_type, note text) RETURNS public.event_instance_registration
    LANGUAGE sql
    AS $_$
  update event_instance_registration eir
  set status = $2, attendance_note = $3
  where eir.id = $1 and eir.person_id is not null and eir.registration_status = 'active'
  returning eir.*;
$_$;
