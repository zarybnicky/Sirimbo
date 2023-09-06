drop policy if exists my_tenant on skupiny;

comment on table person is '@omit create';

create or replace function skupiny_in_current_tenant(s skupiny) returns boolean language sql stable as $$
  select s.tenant_id = current_tenant_id();
$$;
grant all on function skupiny_in_current_tenant to anonymous;
comment on function skupiny_in_current_tenant is '@filterable';

CREATE or replace FUNCTION person_is_admin(p person) RETURNS boolean LANGUAGE sql STABLE security definer AS $$
  select current_tenant_id() = any (auth_details.tenant_administrators) from app_private.auth_details where person_id=p.id;
$$;
GRANT ALL ON FUNCTION person_is_admin TO anonymous;

CREATE or replace FUNCTION person_is_trainer(p person) RETURNS boolean LANGUAGE sql STABLE security definer AS $$
  select current_tenant_id() = any (auth_details.tenant_trainers) from app_private.auth_details where person_id=p.id;
$$;
GRANT ALL ON FUNCTION person_is_admin TO anonymous;

CREATE or replace FUNCTION person_is_member(p person) RETURNS boolean LANGUAGE sql STABLE security definer AS $$
  select current_tenant_id() = any (auth_details.tenant_memberships) from app_private.auth_details where person_id=p.id;
$$;
GRANT ALL ON FUNCTION person_is_member TO anonymous;

comment on table user_proxy is E'@simpleCollections only';
comment on table couple is E'@simpleCollections only';
comment on table cohort_membership is E'@simpleCollections only';
comment on table tenant_membership is E'@simpleCollections only';
comment on table tenant_administrator is E'@simpleCollections only';
comment on table tenant_trainer is E'@simpleCollections only';

comment on table event_registration is E'@omit update
@simpleCollections both';

drop function if exists person_tenant_ids;

drop function if exists event_instances_for_range;
CREATE or replace FUNCTION public.event_instances_for_range(only_mine boolean, only_type event_type, start_range timestamptz, end_range timestamptz default null) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    AS $$
  select event_instance.*
  from event_instance
  join event on event_id=event.id
  where event.is_visible = true
  and tstzrange(start_range, end_range, '[]') && range
  and (only_type is null or event.type = only_type)
  and case only_mine
    when false then true
    else exists (select 1 from event_registration where event_id=event.id and (person_id in (select my_person_ids()) or couple_id in (select my_couple_ids())))
    or exists (select 1 from event_trainer where event_id=event.id and person_id in (select my_person_ids()))
    or exists (select 1 from event_instance_trainer where instance_id=event_instance.id and person_id in (select my_person_ids()))
  end
  order by range asc;
$$;
COMMENT ON FUNCTION public.event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;


create or replace function event_my_registrations(e event) returns setof event_registration language sql stable as $$
  select * from event_registration
  where event_id=e.id
  and (person_id in (select my_person_ids()) or couple_id in (select my_couple_ids()));
$$;
COMMENT ON FUNCTION public.event_my_registrations IS '@simpleCollections only';
grant all on function event_my_registrations to anonymous;

-- insert into person_invitation (person_id, tenant_id, email)
-- select person.id, 2, person.email
-- from app_private.auth_details
--   join person on person.id=auth_details.person_id
--   left join user_proxy on auth_details.person_id=user_proxy.person_id
--   left join person_invitation on person_invitation.person_id=person.id
-- where (2 = any(tenant_memberships) or 2=any(tenant_trainers) or 2=any(tenant_administrators))
--   and (person.email is not null and person.email <> '')
--   and user_id is null and person_invitation.id is null;

comment on table person_invitation is E'@omit update
@simpleCollections only';
grant all on table person_invitation to anonymous;
alter table person_invitation enable row level security;
select app_private.drop_policies('public.person_invitation');
create policy admin_all on person_invitation to administrator using (true);
create policy admin_mine on person_invitation using (person_id in (select my_person_ids()));

drop function if exists login;
CREATE or replace FUNCTION public.login(login character varying, passwd character varying, OUT usr public.users, OUT jwt public.jwt_token) RETURNS record
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  v_salt varchar;
begin
  if login like '%@%' then
    select users.* into usr from users where lower(u_email) = lower(login) limit 1;
  else
    select users.* into usr from users where lower(u_login) = lower(login) limit 1;
  end if;

  if usr is null then
    raise exception 'INVALID_CREDENTIALS' using errcode = '28P01';
  end if;

  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  if usr.u_pass != encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex') then
    raise exception 'INVALID_CREDENTIALS' using errcode = '28P01';
  end if;

  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', array_to_json(jwt.my_person_ids)::text, true);
  perform set_config('jwt.claims.my_tenant_ids', array_to_json(jwt.my_tenant_ids)::text, true);
  perform set_config('jwt.claims.my_cohort_ids', array_to_json(jwt.my_cohort_ids)::text, true);
  perform set_config('jwt.claims.my_couple_ids', array_to_json(jwt.my_couple_ids)::text, true);
  update users set last_login = now() where id = usr.id;
end;
$$;

GRANT ALL ON FUNCTION public.login TO anonymous;
