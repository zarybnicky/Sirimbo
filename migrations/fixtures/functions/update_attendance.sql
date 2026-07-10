create or replace function update_attendance(instance_id bigint, person_id bigint, status attendance_type, note text)
  returns event_instance_registration
  language plpgsql as $$
declare
  eir_row event_instance_registration;
begin
  update event_instance_registration eir
    set status = $3,
        note = $4
  where eir.instance_id = $1 and eir.person_id = $2
  returning eir.* into eir_row;
  return eir_row;
end
$$;

select verify_function('update_attendance');
