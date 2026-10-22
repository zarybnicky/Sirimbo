CREATE FUNCTION public.update_attendance(eir_id bigint, status public.attendance_type, note text) RETURNS public.event_registration
    LANGUAGE sql
    AS $_$
  update event_registration set status = $2, attendance_note = $3 where id = $1 returning *;
$_$;
