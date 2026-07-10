CREATE FUNCTION public.update_event_attendance(instance_id bigint, person_id bigint, status public.attendance_type, note text) RETURNS public.event_attendance
    LANGUAGE plpgsql
    AS $_$
declare
  att event_attendance;
  reg event_registration;
  v_id bigint;
begin
  select event_registration.* into reg
  from event_registration
  join event_instance on event_registration.event_id=event_instance.event_id
  left join couple on couple_id=couple.id
  where event_instance.id=$1 and $2 in (event_registration.person_id, man_id, woman_id);

  update event_instance_registration eir
    set status = $3, note = $4
  where eir.legacy_registration_id = reg.id and eir.instance_id = $1 and eir.person_id = $2
  returning eir.id into v_id;

  select * into att from event_attendance where id = v_id;
  return att;
end
$_$;

GRANT ALL ON FUNCTION public.update_event_attendance(instance_id bigint, person_id bigint, status public.attendance_type, note text) TO anonymous;
