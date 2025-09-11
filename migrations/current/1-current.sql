create or replace function person_has_access(p person) returns boolean stable language sql as $$
  select exists (select 1 from user_proxy where person_id = p.id);
$$;
grant all on function person_has_access to anonymous;

create or replace function people_without_access_with_existing_account() returns setof person stable language sql as $$
  select * from person
  where not exists (select 1 from user_proxy where person_id = person.id)
  and not exists (select 1 from person_invitation where person_id = person.id)
  and exists (select 1 from users where users.u_email = person.email)
  and (exists (select 1 from tenant_membership where active and person_id = person.id and tenant_id = (select current_tenant_id()))
  or exists (select 1 from tenant_trainer where active and person_id = person.id and tenant_id = (select current_tenant_id()))
  or exists (select 1 from tenant_administrator where active and person_id = person.id and tenant_id = (select current_tenant_id())));
$$;
grant all on function people_without_access_with_existing_account to anonymous;

COMMENT ON function people_without_access_with_existing_account IS '@simpleCollections only';

create or replace function people_without_access_or_invitation() returns setof person stable language sql as $$
  select * from person
  where not exists (select 1 from user_proxy where person_id = person.id)
  and not exists (select 1 from person_invitation where person_id = person.id)
  and not exists (select 1 from person_invitation where email = person.email)
  and not exists (select 1 from users where users.u_email = person.email)
  and (exists (select 1 from tenant_membership where active and person_id = person.id and tenant_id = (select current_tenant_id()))
  or exists (select 1 from tenant_trainer where active and person_id = person.id and tenant_id = (select current_tenant_id()))
  or exists (select 1 from tenant_administrator where active and person_id = person.id and tenant_id = (select current_tenant_id())));
$$;
grant all on function people_without_access_or_invitation to anonymous;

COMMENT ON function people_without_access_or_invitation IS '@simpleCollections only';

create or replace function people_without_access_with_invitation() returns setof person stable language sql as $$
  select * from person
  where not exists (select 1 from user_proxy where person_id = person.id)
  and exists (select 1 from person_invitation where person_id = person.id)
  and (exists (select 1 from tenant_membership where active and person_id = person.id and tenant_id = (select current_tenant_id()))
  or exists (select 1 from tenant_trainer where active and person_id = person.id and tenant_id = (select current_tenant_id()))
  or exists (select 1 from tenant_administrator where active and person_id = person.id and tenant_id = (select current_tenant_id())));
$$;
grant all on function people_without_access_with_invitation to anonymous;

COMMENT ON function people_without_access_with_invitation IS '@simpleCollections only';

--!include functions/register_using_invitation.sql
--!include functions/upsert_event.sql
--!include functions/event_instance_approx_price.sql



CREATE or replace FUNCTION invitation_name(token uuid) RETURNS text LANGUAGE sql STABLE SECURITY DEFINER AS $$
  select person_name(person.*)
  from person_invitation join person on person.id=person_id
  where access_token=token and used_at is null;
$$;
GRANT ALL ON FUNCTION invitation_name TO anonymous;


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
