-- Write your migration here

drop function if exists delete_couple;
drop function if exists get_current_couple;

COMMENT ON TABLE public.couple IS '@omit delete';

select app_private.drop_policies('public.address');
alter table person_address drop column if exists address_id;

drop function if exists person_primary_address;
CREATE or replace FUNCTION public.person_primary_address(p public.person) RETURNS public.person_address
    LANGUAGE sql STABLE
    AS $$
  select person_address.* from person_address where person_id = p.id and is_primary = true;
$$;

drop FUNCTION if exists public.logout();
drop FUNCTION if exists public.person_active_couples();

comment on function current_couple_ids is E'@simpleCollections only';

drop table if exists nabidka_item;
drop table if exists nabidka;
drop table if exists rozpis_item;
drop table if exists rozpis;
drop table if exists address;
drop table if exists attendee_user;
drop table if exists pary;
drop table if exists permissions;

drop function if exists confirm_user(bigint, bigint, bigint);
drop function if exists current_session_id;

drop type if exists pary_p_lat_trida;
drop type if exists pary_p_stt_trida;

alter table users drop column if exists u_pohlavi;
alter table users drop column if exists u_telefon;
alter table users drop column if exists u_group;
alter table users drop column if exists u_skupina;
alter table users drop column if exists u_narozeni;
alter table users drop column if exists u_rodne_cislo;
alter table users drop column if exists u_street;
alter table users drop column if exists u_conscription_number;
alter table users drop column if exists u_orientation_number;
alter table users drop column if exists u_district;
alter table users drop column if exists u_city;
alter table users drop column if exists u_postal_code;
alter table users drop column if exists u_lock;
alter table users drop column if exists u_level;
alter table users drop column if exists u_dancer;
alter table users drop column if exists u_teacher;

drop type if exists jwt_token cascade;
create type jwt_token as (
  exp integer,
  user_id bigint,
  tenant_id bigint,
  username text,
  email text,
  my_person_ids bigint[],
  my_tenant_ids bigint[],
  my_cohort_ids bigint[],
  my_couple_ids bigint[],
  is_member boolean,
  is_trainer boolean,
  is_admin boolean
);

DO $$ BEGIN
  IF NOT EXISTS (SELECT FROM pg_catalog.pg_aggregate join pg_proc on aggfnoid=pg_proc.oid WHERE proname='array_accum') THEN
    CREATE AGGREGATE array_accum (anycompatiblearray) (
      sfunc = array_cat,
      stype = anycompatiblearray,
      initcond = '{}'
    );
  END IF;
END $$;

create or replace function app_private.create_jwt_token(u users) returns jwt_token language sql stable as $$
  with details as (
    SELECT
      user_id,
      person.id as person_id,
      (select array_agg(tenant_id) from tenant_membership where person_id=person.id) as my_tenant_ids,
      (select array_agg(cohort_id) from cohort_membership where person_id=person.id) as my_cohort_ids,
      (select array_agg(id) from couple where man_id=person.id or woman_id=person.id) as my_couple_ids,
      exists (select 1 from tenant_membership where tenant_membership.person_id=person.id and tenant_membership.tenant_id = current_tenant_id() and now() <@ active_range) as is_member,
      exists (select 1 from tenant_trainer where tenant_trainer.person_id=person.id and tenant_trainer.tenant_id = current_tenant_id() and now() <@ active_range) as is_trainer,
      exists (select 1 from tenant_administrator where tenant_administrator.person_id=person.id and tenant_administrator.tenant_id = current_tenant_id() and now() <@ active_range) as is_admin
    from user_proxy join person on person_id=person.id where user_id=u.u_id
  ) select
    extract(epoch from now() + interval '7 days')::integer,
    u.u_id,
    current_tenant_id(),
    u.u_login,
    u.u_email,
    array_agg(person_id) as my_person_ids,
    array_accum(my_tenant_ids) as my_tenant_ids,
    array_accum(my_cohort_ids) as my_cohort_ids,
    array_accum(my_couple_ids) as my_couple_ids,
    bool_or(is_member) as is_member,
    bool_or(is_trainer) as is_trainer,
    bool_or(is_admin) as is_admin
  from details group by user_id;
$$;

drop function if exists register_using_invitation;
create or replace function register_using_invitation(email text, login text, passwd text, token uuid, out usr users, out sess session, out jwt jwt_token) language plpgsql strict security definer as $$
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

  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  insert into users (u_login, u_email, u_pass) values (login, email, encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex')) returning * into usr;
  insert into user_proxy (user_id, person_id) values (usr.id, invitation.person_id);
  update person_invitation set used_at=now() where access_token=token;
  insert into session (ss_id, ss_user, ss_lifetime) values (gen_random_uuid(), usr.u_id, 86400)
  returning * into sess;
  jwt := app_private.create_jwt_token(usr);
end
$$;
--select verify_function('register_using_invitation');
grant all on function register_using_invitation to anonymous;

drop function if exists login;
CREATE FUNCTION public.login(login character varying, passwd character varying, OUT sess public.session, OUT usr public.users, out jwt jwt_token) RETURNS record
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
    raise exception 'ACCOUNT_NOT_FOUND' using errcode = '28000';
  end if;

  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  if usr.u_pass != encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex') then
    raise exception 'INVALID_PASSWORD' using errcode = '28P01';
  end if;

  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', array_to_json(jwt.my_person_ids)::text, true);
  perform set_config('jwt.claims.my_tenant_ids', array_to_json(jwt.my_tenant_ids)::text, true);
  perform set_config('jwt.claims.my_cohort_ids', array_to_json(jwt.my_cohort_ids)::text, true);
  perform set_config('jwt.claims.my_couple_ids', array_to_json(jwt.my_couple_ids)::text, true);
  insert into session (ss_id, ss_user, ss_lifetime) values (gen_random_uuid(), usr.u_id, 86400)
  returning * into sess;
end;
$$;
--select verify_function('login');
grant all on function login to anonymous;

CREATE or replace FUNCTION my_person_ids() RETURNS setof bigint LANGUAGE sql STABLE AS $$
  SELECT json_array_elements_text(nullif(current_setting('jwt.claims.my_person_ids', true), '')::json)::bigint;
$$;
GRANT ALL ON FUNCTION my_person_ids() TO anonymous;
comment on function my_person_ids is '@omit';

CREATE or replace FUNCTION my_tenant_ids() RETURNS setof bigint LANGUAGE sql STABLE AS $$
  SELECT json_array_elements_text(nullif(current_setting('jwt.claims.my_tenant_ids', true), '')::json)::bigint;
$$;
GRANT ALL ON FUNCTION my_tenant_ids() TO anonymous;
comment on function my_tenant_ids is '@omit';

CREATE or replace FUNCTION my_cohort_ids() RETURNS setof bigint LANGUAGE sql STABLE AS $$
  SELECT json_array_elements_text(nullif(current_setting('jwt.claims.my_cohort_ids', true), '')::json)::bigint;
$$;
GRANT ALL ON FUNCTION my_cohort_ids() TO anonymous;
comment on function my_cohort_ids is '@omit';

CREATE or replace FUNCTION my_couple_ids() RETURNS setof bigint LANGUAGE sql STABLE AS $$
  SELECT json_array_elements_text(nullif(current_setting('jwt.claims.my_couple_ids', true), '')::json)::bigint;
$$;
GRANT ALL ON FUNCTION my_couple_ids() TO anonymous;
comment on function my_couple_ids is '@omit';

select app_private.drop_policies('public.person');
create policy admin_all on person to administrator using (true);
create policy admin_myself on person for update using (id in (select my_person_ids()));
create policy view_same_tenant on person for select using (exists (select 1 from tenant_membership where now() <@ active_range and person_id = id and tenant_id in (select my_tenant_ids())));
create policy view_tenant_admin on person for select using (exists (select 1 from tenant_administrator where now() <@ active_range and person_id = id));
create policy view_tenant_trainer on person for select using (exists (select 1 from tenant_trainer where now() <@ active_range and person_id = id));

create or replace function invitation_info(token uuid) returns text language sql stable security definer as $$
  select person.email from person_invitation join person on person.id=person_id where access_token=token and used_at is null;
$$;
grant all on function invitation_info to anonymous;

drop index if exists couple_range_idx;
drop index if exists cohort_membership_range_idx;
drop index if exists tenant_membership_range_idx;
drop index if exists tenant_trainer_range_idx;
drop index if exists tenant_administrator_range_idx;
CREATE INDEX if not exists couple_range_idx ON public.couple USING gist (active_range, man_id, woman_id);
CREATE INDEX if not exists cohort_membership_range_idx ON public.cohort_membership USING gist (active_range, tenant_id, person_id);
CREATE INDEX if not exists tenant_membership_range_idx ON public.tenant_membership USING gist (active_range, tenant_id, person_id);
CREATE INDEX if not exists tenant_trainer_range_idx ON public.tenant_trainer USING gist (active_range, tenant_id, person_id);
CREATE INDEX if not exists tenant_administrator_range_idx ON public.tenant_administrator USING gist (active_range, tenant_id, person_id);

alter table person add column if not exists prefix_title text not null default '';
alter table person add column if not exists suffix_title text not null default '';
alter table person add column if not exists bio text not null default '';

create or replace function person_name(p person) returns text language sql stable as $$
  select concat_ws(' ', p.prefix_title, p.first_name, p.last_name) || (case p.suffix_title when '' then '' else ', ' || p.suffix_title end);
$$;
grant all on function person_name to anonymous;

alter table tenant drop column if exists member_info;
alter table tenant add column if not exists description text not null default '';
alter table skupiny drop column if exists internal_info;

CREATE EXTENSION IF NOT EXISTS citext;

alter table person add column if not exists email citext null default null;
alter table person add column if not exists phone text null default null;

do $$
begin
  update person set email = person_email.email
  from person_email where person.id=person_email.person_id;
  update person set phone = person_phone.phone
  from person_phone where person.id=person_phone.person_id;
end
$$;

comment on table person_email is '@omit';
comment on table person_phone is '@omit';
drop function if exists person_primary_email;
drop function if exists person_primary_phone;

drop function if exists create_person;
create or replace function public.create_person(inout p person, is_member boolean, is_trainer boolean, is_admin boolean, send_invitation boolean, join_date timestamptz) language plpgsql as $$
begin
  insert into person overriding user value select p.* returning * into p;
  if is_member = true then
    insert into tenant_membership (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_trainer = true then
    insert into tenant_trainer (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_admin = true then
    insert into tenant_administrator (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if send_invitation = true then
    insert into person_invitation (person_id, tenant_id) values (p.id, current_tenant_id());
  end if;
end
$$;
select verify_function('create_person');
grant all on function create_person to anonymous;

comment on table cohort_membership is E'@omit delete
@simpleCollections only';
comment on table tenant_membership is E'@omit delete
@simpleCollections only';
comment on table tenant_administrator is E'@omit delete
@simpleCollections only';
comment on table tenant_trainer is E'@omit delete
@simpleCollections only';

CREATE FUNCTION public.person_couples(p public.person) RETURNS SETOF public.couple
    LANGUAGE sql STABLE
    AS $$
  select couple.* from couple where (man_id = p.id or woman_id = p.id) and now() <@ active_range order by active_range;
$$;
