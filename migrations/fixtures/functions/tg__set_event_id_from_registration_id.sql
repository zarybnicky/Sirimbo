create or replace function app_private.tg__set_event_id_from_registration_id()
  returns trigger
  language plpgsql
as $$
begin
  select registration.event_id into new.event_id
  from public.event_instance_registration registration
  where registration.id = new.registration_id;
  return new;
end;
$$;

select verify_function(
  'app_private.tg__set_event_id_from_registration_id',
  'public.event_lesson_demand'
);
