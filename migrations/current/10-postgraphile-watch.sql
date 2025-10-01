
create or replace function postgraphile_watch.notify_watchers_ddl() returns event_trigger as $$
declare
  ddl_commands json;
begin
  select json_agg(
    json_build_object('schema', schema_name, 'command', command_tag)
  ) into ddl_commands
  from pg_event_trigger_ddl_commands();

  if json_array_length(ddl_commands) > 0 then
    perform pg_notify(
      'postgraphile_watch',
      json_build_object('type', 'ddl', 'payload', ddl_commands)::text
    );
  end if;
end;
$$ language plpgsql;

create or replace function postgraphile_watch.notify_watchers_drop() returns event_trigger as $$
declare
  objects json;
begin
  select json_agg(distinct schema_name) into objects
  from pg_event_trigger_dropped_objects()
  where schema_name <> 'pg_temp';

  if json_array_length(objects) > 0 then
    perform pg_notify(
      'postgraphile_watch',
      json_build_object('type', 'drop', 'payload', objects)::text
    );
  end if;
end;
$$ language plpgsql;
