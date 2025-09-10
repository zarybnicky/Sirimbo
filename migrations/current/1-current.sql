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


drop function if exists register_using_invitation(email text, login text, passwd text, token uuid);
drop function if exists register_using_invitation(email text, passwd text, token uuid, login text);
CREATE or replace FUNCTION public.register_using_invitation(email text, passwd text, token uuid, login text default null, OUT usr public.users, OUT jwt public.jwt_token) RETURNS record
    LANGUAGE plpgsql SECURITY DEFINER
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
