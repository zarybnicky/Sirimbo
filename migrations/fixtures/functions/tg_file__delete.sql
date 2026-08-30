create or replace function app_private.tg_file__delete()
returns trigger
language plpgsql
security definer
set search_path to pg_catalog, public, pg_temp
as $$
begin
  perform graphile_worker.add_job(
    'delete_file',
    json_build_object('object_key', old.object_key)
  );
  return old;
end;
$$;

drop trigger if exists _900_delete_object on file;
create trigger _900_delete_object
  after delete on file
  for each row execute function app_private.tg_file__delete();
