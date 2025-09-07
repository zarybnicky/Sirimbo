create or replace function person_has_access(p person) returns boolean stable language sql as $$
  select exists (select 1 from user_proxy where person_id = p.id);
$$;
grant all on function person_has_access to anonymous;

create or replace function people_without_access_with_existing_account() returns setof person stable language sql as $$
  select * from person
  where not exists (select 1 from user_proxy where person_id = person.id)
  and not exists (select 1 from person_invitation where person_id = person.id)
  and exists (select 1 from users where users.u_email = person.email)
  and exists (select 1 from tenant_membership where active and person_id = person.id and tenant_id = (select current_tenant_id()));
$$;
grant all on function people_without_access_with_existing_account to anonymous;

COMMENT ON function people_without_access_with_existing_account IS '@simpleCollections only';

create or replace function people_without_access_or_invitation() returns setof person stable language sql as $$
  select * from person
  where not exists (select 1 from user_proxy where person_id = person.id)
  and not exists (select 1 from person_invitation where person_id = person.id)
  and not exists (select 1 from person_invitation where email = person.email)
  and not exists (select 1 from users where users.u_email = person.email)
  and exists (select 1 from tenant_membership where active and person_id = person.id and tenant_id = (select current_tenant_id()));
$$;
grant all on function people_without_access_or_invitation to anonymous;

COMMENT ON function people_without_access_or_invitation IS '@simpleCollections only';

create or replace function people_without_access_with_invitation() returns setof person stable language sql as $$
  select * from person
  where not exists (select 1 from user_proxy where person_id = person.id)
  and exists (select 1 from person_invitation where person_id = person.id)
  and exists (select 1 from tenant_membership where active and person_id = person.id and tenant_id = (select current_tenant_id()));
$$;
grant all on function people_without_access_with_invitation to anonymous;

COMMENT ON function people_without_access_with_invitation IS '@simpleCollections only';


drop function if exists register_using_invitation(email text, login text, passwd text, token uuid);
drop function if exists register_using_invitation(email text, passwd text, token uuid, login text);
CREATE or replace FUNCTION public.register_using_invitation(email text, passwd text, token uuid, login text default null, OUT usr public.users, OUT jwt public.jwt_token) RETURNS record
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  invitation person_invitation;
  v_salt text;
begin
  select * into invitation from person_invitation where access_token=token;

  if invitation is null then
    raise exception 'INVITATION_NOT_FOUND' using errcode = '28000';
  end if;
  if invitation.used_at is not null then
    raise exception 'INVITATION_ALREADY_USED' using errcode = '28P01';
  end if;
  if email is null or email = '' then
    raise exception 'INVALID_EMAIL' using errcode = '28P01';
  end if;

  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  insert into users (u_login, u_email, u_pass) values (trim(login), email, encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex')) returning * into usr;
  insert into user_proxy (user_id, person_id) values (usr.id, invitation.person_id);
  update person_invitation set used_at=now() where access_token=token;
  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);
end
$$;

GRANT ALL ON FUNCTION public.register_using_invitation TO anonymous;












create or replace function postgraphile_watch.notify_watchers_ddl() returns event_trigger as $$
declare
  ddl_commands json;
begin
  select json_agg(json_build_object('schema', schema_name, 'command', command_tag)) into ddl_commands
  from pg_event_trigger_ddl_commands();

  if json_array_length(ddl_commands) > 0 then
    perform pg_notify(
      'postgraphile_watch',
      json_build_object('type', 'ddl', 'payload', ddl_commands)::text
    );
  end if;
end;
$$ language plpgsql;

select * from plpgsql_check_function('postgraphile_watch.notify_watchers_ddl');

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

select * from plpgsql_check_function('postgraphile_watch.notify_watchers_drop');
