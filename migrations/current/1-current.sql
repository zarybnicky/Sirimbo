
drop function if exists person_primary_address;
drop table if exists person_address;
drop table if exists person_email;
drop table if exists person_phone;

grant all on function is_current_tenant_member to anonymous;
comment on table event is E'@omit create';

drop function if exists create_event;
create or replace function create_event(
  inout info event,
  instances event_instance[],
  trainers event_trainer[],
  cohorts event_target_cohort[],
  registrations event_registration[]
) language plpgsql as $$
begin
  insert into event (name, summary, description, type, location_text, capacity, is_visible, is_public, is_locked, enable_notes)
  select info.name, info.summary, info.description, info.type, info.location_text, info.capacity, info.is_visible, info.is_public, info.is_locked, info.enable_notes
  returning * into info;

  insert into event_instance (event_id, since, until)
  select info.id, since, until from unnest(instances) i;

  insert into event_trainer (event_id, person_id, lessons_offered)
  select info.id, person_id, coalesce(lessons_offered, 0) from unnest(trainers) i;

  insert into event_target_cohort (event_id, cohort_id)
  select info.id, cohort_id from unnest(cohorts) i;

  insert into event_registration (event_id, person_id, couple_id, is_confirmed)
  select info.id, person_id, couple_id, true from unnest(registrations) i;
end;
$$;
select verify_function('create_event');
grant all on function create_event to anonymous;
comment on function create_event is E'@arg0variant create
@arg1variant patch
@arg2variant patch
@arg3variant patch
@arg4variant patch
';

create or replace function tenant_couples(t tenant) returns setof couple language sql stable as $$
  select distinct couple.*
  from couple
  join tenant_membership on man_id=person_id or woman_id=person_id
  where now() <@ couple.active_range and now() <@ tenant_membership.active_range and tenant_id=t.id
  order by couple.active_range asc;
$$;
grant all on function tenant_couples to anonymous;
comment on function tenant_couples is '@simpleCollections only';

do $$
begin
if exists (select 1 from pg_sequences where sequencename='akce_a_id_seq') then
  ALTER TABLE public.event ALTER id DROP DEFAULT;
  DROP SEQUENCE public.akce_a_id_seq;
  ALTER TABLE public.event ALTER id ADD GENERATED ALWAYS AS IDENTITY;
  perform setval(pg_get_serial_sequence('event', 'id'), (select max(id) from event));
end if;
end
$$;

do $$ begin
  if not exists (SELECT 1 fROM pg_type JOIN pg_enum ON pg_type.oid = pg_enum.enumtypid WHERE typname = 'event_type' and enumlabel = 'group') then
    alter type event_type add value 'group';
  end if;
end $$;

drop function if exists event_instances_for_range;
CREATE or replace FUNCTION public.event_instances_for_range(only_mine boolean, only_type event_type, start_range timestamptz, end_range timestamptz default null) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    AS $$
  select event_instance.* from event_instance join event on event_id=event.id
  where event.is_visible = true and tstzrange(start_range, end_range, '[]') && range and (only_type is null or event.type = only_type)
  and case only_mine
    when false then true
    else exists (select 1 from event_registration where event_id=event.id and (person_id in (select my_person_ids()) or couple_id in (select my_couple_ids()))) end
  order by range asc;
$$;
COMMENT ON FUNCTION public.event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;

create or replace function move_event_instance(id bigint, since timestamptz, until timestamptz, trainer_person_id bigint) returns event_instance language plpgsql as $$
declare
  v_id alias for move_event_instance.id;
  inst event_instance;
begin
  select * from event_instance into inst where event_instance.id = move_event_instance.id;
  if trainer_person_id is not null then
    if (select count(*) = 1 from event_instance_trainer where instance_id = inst.id) then
      update event_instance_trainer set person_id = trainer_person_id where instance_id = inst.id;
    elsif (select count(*) = 1 from event_trainer where event_id = inst.event_id) then
      update event_trainer set person_id = trainer_person_id where event_id = inst.event_id;
    end if;
  end if;
  update event_instance set since=move_event_instance.since, until=move_event_instance.until where event_instance.id=inst.id
  returning * into inst;
  return inst;
end;
$$;
select verify_function('move_event_instance');
grant all on function move_event_instance to anonymous;

ALTER TABLE event ALTER COLUMN capacity TYPE integer USING capacity::integer;

comment on function couple_attendances is E'@simpleCollections only
@filterable
@sortable
@deprecated';

CREATE or replace FUNCTION public.couple_event_instances(p couple) RETURNS SETOF public.event_instance LANGUAGE sql STABLE AS $$
  select distinct event_instance.*
  from event_instance
  join event_registration on event_instance.event_id=event_registration.event_id
  where couple_id = p.id;
$$;
GRANT ALL ON FUNCTION public.couple_event_instances(couple) TO anonymous;
comment on function couple_event_instances is E'@simpleCollections only
@filterable
@sortable';

alter table person_invitation add column if not exists email citext not null;
alter table users alter column u_jmeno drop not null;
alter table users alter column u_prijmeni drop not null;
alter table users alter column u_nationality drop not null;

drop trigger if exists _500_notify_admin on users;
drop function if exists app_private.tg_users__notify_admin();
drop policy if exists register_anonymous on users;
drop policy if exists my_tenant on users;

alter table users add column if not exists last_login timestamptz null;

CREATE or replace FUNCTION public.login(login character varying, passwd character varying, OUT sess public.session, OUT usr public.users, OUT jwt public.jwt_token) RETURNS record LANGUAGE plpgsql STRICT SECURITY DEFINER AS $$
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
  insert into session (ss_id, ss_user, ss_lifetime) values (gen_random_uuid(), usr.u_id, 86400)
  returning * into sess;
  update users set last_login = now() where id = usr.id;
end;
$$;
select plpgsql_check_function('public.login');

alter table user_proxy
  add column if not exists since timestamptz null,
  add column if not exists until timestamptz null,
  add column if not exists active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL;
create or replace function user_proxy_active(c user_proxy) returns boolean language sql stable as $$
  select now() <@ c.active_range;
$$;
grant all on function user_proxy_active to anonymous;
comment on function user_proxy_active is E'@filterable';
comment on column user_proxy.active_range is E'@omit';
CREATE INDEX if not exists user_proxy_range_idx ON user_proxy USING gist (active_range, person_id, user_id);
comment on table user_proxy is '@omit delete
@simpleCollections only';

drop function if exists create_person;
CREATE or replace FUNCTION public.create_person(person_id bigint, INOUT p public.person, is_member boolean, is_trainer boolean, is_admin boolean, send_invitation boolean, join_date timestamp with time zone) RETURNS public.person
    LANGUAGE plpgsql
    AS $$
begin
  if person_id is null then
    insert into person overriding user value select p.* returning * into p;
  else
    select * into p from person where person.id=person_id;
  end if;
  if is_member = true then
    insert into tenant_membership (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_trainer = true then
    insert into tenant_trainer (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_admin = true then
    insert into tenant_administrator (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if send_invitation = true and p.email is not null and p.email <> '' then
    insert into person_invitation (person_id, tenant_id, email) values (p.id, current_tenant_id(), p.email);
  end if;
end
$$;
select verify_function('create_person');
grant all on function create_person to anonymous;

select app_private.drop_policies('public.event');
CREATE POLICY view_public ON public.event FOR SELECT TO anonymous USING (is_public = true or tenant_id in (select my_tenant_ids()));
CREATE POLICY admin_same_tenant ON public.event to administrator USING (tenant_id IN (SELECT my_tenant_ids()));
CREATE POLICY my_tenant ON public.event AS RESTRICTIVE USING (tenant_id = current_tenant_id());

alter table skupiny alter column s_description set default '';

drop view if exists app_private.auth_details;
create or replace view app_private.auth_details as
  SELECT
    person.id as person_id,
    array_agg(cohort_id) as cohort_memberships,
    array_agg(tenant_membership.tenant_id) tenant_memberships,
    array_agg(tenant_trainer.tenant_id) tenant_trainers,
    array_agg(tenant_administrator.tenant_id) tenant_administrators
  from person
  left join cohort_membership on person.id=cohort_membership.person_id and now() <@ cohort_membership.active_range
  left join tenant_membership on person.id=tenant_membership.person_id and now() <@ tenant_membership.active_range
  left join tenant_trainer on person.id=tenant_trainer.person_id and now() <@ tenant_trainer.active_range
  left join tenant_administrator on person.id=tenant_administrator.person_id and now() <@ tenant_administrator.active_range
  group by person.id;

drop function if exists filtered_people;
CREATE or replace FUNCTION public.filtered_people(in_tenants bigint[], in_cohort bigint, is_trainer boolean, is_admin boolean) RETURNS SETOF person LANGUAGE sql STABLE security definer AS $$
  select person.* from person
  join app_private.auth_details on person_id=person.id
  where
  (    current_tenant_id() = any (auth_details.tenant_memberships)
    OR current_tenant_id() = any (auth_details.tenant_trainers)
    OR current_tenant_id() = any (auth_details.tenant_administrators)
  )
  and case when in_cohort is null then true else in_cohort = any (auth_details.cohort_memberships) end
  and case when is_trainer is null then true else is_trainer = (current_tenant_id() = any (auth_details.tenant_trainers)) end
  and case when is_admin is null then true else is_admin = (current_tenant_id() = any (auth_details.tenant_administrators)) end
  order by last_name, first_name
$$;
COMMENT ON FUNCTION public.filtered_people IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.filtered_people TO anonymous;
