--! include functions/update_event_instance_details.sql

alter function app_private.sync_eir_registrations(bigint[])
  set search_path = pg_catalog, public, pg_temp;
