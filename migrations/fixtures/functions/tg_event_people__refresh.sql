create or replace function app_private.tg_event_people__refresh() returns trigger
language plpgsql security definer
set search_path = pg_catalog, public, pg_temp
as $$
begin
  perform graphile_worker.add_job(
    'refresh_event_people',
    queue_name := 'refresh_event_people',
    job_key := 'refresh_event_people'
  );
  return null;
end;
$$;

create or replace trigger _900_refresh_event_people
after insert or delete or update of tenant_id, instance_id, person_id, registration_status
on event_instance_registration
for each statement execute function app_private.tg_event_people__refresh();

create or replace trigger _900_refresh_event_people
after insert or delete or update of tenant_id, instance_id, person_id
on event_instance_trainer
for each statement execute function app_private.tg_event_people__refresh();

create or replace trigger _900_refresh_event_people
after insert or delete or update of tenant_id, parent_id
on event_instance
for each statement execute function app_private.tg_event_people__refresh();
