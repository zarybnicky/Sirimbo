CREATE or replace FUNCTION update_event_attendance(instance_id bigint, person_id bigint, status attendance_type, note text) RETURNS event_attendance
    LANGUAGE sql
    AS $$
  update event_instance_registration eir
  set status = $3, attendance_note = $4
  where eir.id = (
    select candidate.id
    from event_instance_registration candidate
    where candidate.instance_id = $1
      and candidate.person_id = $2
      and candidate.legacy_registration_id is not null
    order by candidate.id
    limit 1
  )
  returning eir.id, eir.tenant_id, eir.instance_id, eir.person_id, eir.status,
    eir.attendance_note, eir.legacy_registration_id, eir.event_id,
    eir.attendance_created_at, eir.attendance_updated_at;
$$;

GRANT ALL ON FUNCTION update_event_attendance(instance_id bigint, person_id bigint, status attendance_type, note text) TO anonymous;
