--! Previous: sha1:1b19dc580423b5e0017cd48b9223122b942407c9
--! Hash: sha1:c5e51c419f94c1e8c8130b3c78b0c4619688c75f

--! split: 1-cleanup.sql
drop function if exists event_has_capacity;
drop function if exists event_my_notes;
drop function if exists event_signed_up;
drop function if exists event_is_future;
drop function if exists my_lessons;
drop function if exists crm_copy_to_form_responses cascade;
drop function if exists regenerate_event_registration_from_attendees;
drop function if exists app_private.insert_revision;
drop function if exists app_private.regenerate_event_lesson;
drop function if exists app_private.regenerate_event_reservation;
drop function if exists app_private.regenerate_table_couple;
drop function if exists app_private.regenerate_table_person;
drop function if exists on_delete_file_dokumenty cascade;
drop function if exists trainers;
drop function if exists users_full_name;

drop table if exists attendee_external;
alter table if exists app_private.attendee_user set schema public;
alter table if exists app_private.nabidka set schema public;
alter table if exists app_private.rozpis set schema public;
alter table if exists app_private.nabidka_item set schema public;
alter table if exists app_private.rozpis_item set schema public;
alter table if exists app_private.pary set schema public;

comment on table public.pary is E'@omit';

alter table event alter column since drop not null;
alter table event alter column until drop not null;

do $$ begin
  if not exists (select 1 from information_schema.columns where table_name = 'event_instance' and column_name = 'since') then
    alter table event_instance add since timestamptz;
    alter table event_instance add until timestamptz;
    update event_instance set since=lower(range), until=upper(range);
    alter table event_instance alter since set not null;
    alter table event_instance alter until set not null;
    alter table event_instance drop if exists range;
    alter table event_instance add range tstzrange not null generated always as (tstzrange(since, until, '[]')) stored;
    CREATE INDEX event_instance_range_idx ON event_instance USING gist (range);
    CREATE INDEX event_instance_since_idx ON event_instance (since);
    CREATE INDEX event_instance_until_idx ON event_instance (until);
  end if;
end $$;

drop function if exists event_instances_for_range;
CREATE or replace FUNCTION public.event_instances_for_range(only_mine boolean, type event_type, start_range timestamptz, end_range timestamptz default null) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    AS $$
  select event_instance.* from event_instance join event on event_id=event.id
  where event.is_visible = true and tstzrange(start_range, end_range, '[]') && range and (type is null or event.type = type)
  and case only_mine
    when false then true
    else exists (select 1 from event_registration where event_id=event.id and (person_id in (select my_person_ids()) or couple_id in (select my_couple_ids()))) end
  order by range asc;
$$;
COMMENT ON FUNCTION public.event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;

drop function if exists filtered_people;
create or replace function filtered_people(in_tenants bigint[], in_cohort bigint, is_trainer boolean, is_admin boolean) returns setof person language sql stable as $$
 select person.* from person
  where
    exists (select 1 from tenant_membership where tenant_id = any (in_tenants) and person_id=person.id and active=true)
  and
    case when in_cohort is null then true
    else exists (select 1 from cohort_membership where cohort_id=in_cohort and person_id=person.id and active=true) end
  and
    case when is_trainer = false then true
    else exists (select 1 from tenant_trainer where tenant_id = any (in_tenants) and person_id=person.id) end
  and
    case when is_admin = false then true
    else exists (select 1 from tenant_administrator where tenant_id = any (in_tenants) and person_id=person.id) end
$$;
COMMENT ON FUNCTION filtered_people IS '@simpleCollections only';
GRANT ALL ON FUNCTION filtered_people TO anonymous;

create or replace function edit_registration(registration_id bigint, note text) returns event_registration language plpgsql strict as $$
#variable_conflict use_variable
declare
  event event;
  reg event_registration;
begin
  select * into reg from event_registration er where er.id = registration_id;
  select * into event from event where id = reg.event_id;

  if event is null or reg is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if reg.person_id not in (select my_person_ids()) and reg.couple_id not in (select my_couple_ids()) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  update event_registration set note=note where id = reg.id returning * into reg;
  return reg;
end;
$$;
select verify_function('public.edit_registration');
GRANT ALL ON FUNCTION public.edit_registration TO anonymous;

CREATE or replace FUNCTION my_person_ids() RETURNS SETOF bigint LANGUAGE sql STABLE security definer AS $$
  select person.id
  from person join user_proxy on person_id=person.id
  where user_id = current_user_id();
$$;
GRANT ALL ON FUNCTION my_person_ids() TO anonymous;
comment on function my_person_ids is '@omit';

CREATE or replace FUNCTION person_couple_ids(p person) RETURNS bigint[] LANGUAGE sql STABLE AS $$
  select array_agg(couple.id)
  from couple
  where man_id = p.id or woman_id = p.id and active = true;
$$;
GRANT ALL ON FUNCTION person_couple_ids TO anonymous;

CREATE or replace FUNCTION my_couple_ids() RETURNS SETOF bigint LANGUAGE sql STABLE security definer AS $$
  select couple.id
  from couple join person on (man_id = person.id or woman_id = person.id) join user_proxy on person_id=person.id
  where user_id = current_user_id() and active = true;
$$;
GRANT ALL ON FUNCTION my_couple_ids() TO anonymous;
comment on function my_couple_ids is '@omit';

CREATE or replace FUNCTION person_tenant_ids(p person) RETURNS bigint[] LANGUAGE sql STABLE AS $$
  select array_agg(tenant_id) from tenant_membership where active = true and person_id = p.id;
$$;
GRANT ALL ON FUNCTION person_tenant_ids TO anonymous;

CREATE or replace FUNCTION person_cohort_ids(p person) RETURNS bigint[] LANGUAGE sql STABLE AS $$
  select array_agg(cohort_id) from cohort_membership where active = true and person_id = p.id;
$$;
GRANT ALL ON FUNCTION person_cohort_ids TO anonymous;

CREATE or replace FUNCTION my_tenant_ids() RETURNS SETOF bigint LANGUAGE sql STABLE security definer AS $$
  select tenant.id
  from tenant join tenant_membership on tenant.id = tenant_id
  where tenant_membership.active = true and person_id in (select my_person_ids());
$$;
GRANT ALL ON FUNCTION my_tenant_ids() TO anonymous;
comment on function my_tenant_ids is '@omit';

CREATE or replace FUNCTION public.person_couples(p person) RETURNS SETOF public.couple LANGUAGE sql STABLE AS $$
  select couple.* from couple where (man_id = p.id or woman_id = p.id) and active = true;
$$;
GRANT ALL ON FUNCTION public.person_couples(person) TO anonymous;
comment on function person_couples is E'@simpleCollections only';

CREATE or replace FUNCTION person_is_admin(p person) RETURNS boolean LANGUAGE sql STABLE AS $$
  select exists (select 1 from tenant_administrator where active = true and person_id = p.id);
$$;
GRANT ALL ON FUNCTION person_is_admin TO anonymous;
CREATE or replace FUNCTION person_is_trainer(p person) RETURNS boolean LANGUAGE sql STABLE AS $$
  select exists (select 1 from tenant_trainer where active = true and person_id = p.id);
$$;
GRANT ALL ON FUNCTION person_is_admin TO anonymous;


drop function if exists register_to_event;
create or replace function register_to_event(inout registration event_registration, lessons event_lesson_demand[]) language plpgsql strict security definer as $$
declare
  event event;
  demand event_lesson_demand;
begin
  select * into event from event where id = registration.event_id;

  if event is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if registration.person_id not in (select my_person_ids()) and registration.couple_id not in (select my_couple_ids()) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  insert into event_registration (event_id, person_id, couple_id, note) select registration.event_id, registration.person_id, registration.couple_id, registration.note returning * into registration;
  foreach demand in array lessons loop
    perform set_lesson_demand(registration.id, demand.trainer_id, demand.lesson_count);
  end loop;
end;
$$;
comment on function register_to_event is E'@arg0variant create
@arg1variant patch';
select verify_function('public.register_to_event');
GRANT ALL ON FUNCTION public.register_to_event TO anonymous;

drop function if exists public.set_lesson_demand;
CREATE or replace FUNCTION public.set_lesson_demand(
  registration_id bigint,
  trainer_id bigint,
  lesson_count int
) RETURNS public.event_lesson_demand LANGUAGE plpgsql STRICT security definer AS $$
#variable_conflict use_variable
declare
  event event;
  registration event_registration;
  current_lessons int;
  lesson_demand event_lesson_demand;
begin
  select * into registration from event_registration where id = registration_id;
  select * into event from event where id = registration.event_id;
  select sum(lesson_count)::int into current_lessons from event_lesson_demand eld where eld.registration_id = registration_id;

  if lesson_count = 0 then
    delete from event_lesson_demand eld where eld.registration_id = registration.id and eld.trainer_id = trainer_id;
    return null;
  end if;

  if lesson_count > (current_lessons + event_remaining_lessons(event)) then
    select (current_lessons + event_remaining_lessons(event)) into lesson_count;
  end if;

  INSERT INTO event_lesson_demand (registration_id, trainer_id, lesson_count)
  values (registration.id, trainer_id, lesson_count)
  on conflict on constraint eld_unique_registration_trainer_key do update set lesson_count = lesson_count
  returning * into lesson_demand;

  return lesson_demand;
end;
$$;
select verify_function('public.set_lesson_demand');
GRANT ALL ON FUNCTION public.set_lesson_demand TO anonymous;

CREATE or replace FUNCTION public.event_trainer_lessons_remaining(e public.event_trainer) RETURNS integer LANGUAGE sql STABLE AS $$
  select e.lessons_offered - (select coalesce(sum(lesson_count), 0) from event_lesson_demand where trainer_id = e.id);
$$;
CREATE or replace FUNCTION public.event_remaining_person_spots(e public.event) RETURNS integer LANGUAGE sql STABLE security definer AS $$
  select e.capacity - (select coalesce(sum(case when couple_id is not null then 2 else 1 end), 0) from event_registration where event_id = e.id);
$$;
CREATE or replace FUNCTION public.event_remaining_lessons(e public.event) RETURNS integer LANGUAGE sql STABLE security definer AS $$
  select (select coalesce(sum(lessons_offered), 0) from event_trainer where event_id = e.id) - (select coalesce(sum(lesson_count), 0) from event_registration join event_lesson_demand on registration_id = event_registration.id where event_id = e.id);
$$;

select app_private.drop_policies('public.event_registration');
create policy admin_all on event_registration to administrator using (true);
create policy view_visible_event on event_registration for select using (
  exists (select 1 from event where event_id = event.id)
  and (
     exists (select 1 from person where person_id = person.id)
     or exists (select 1 from couple where couple_id = couple.id)
  )
);

drop function if exists public.login;
CREATE FUNCTION public.login(login character varying, passwd character varying, OUT sess public.session, OUT usr public.users) RETURNS record LANGUAGE plpgsql STRICT SECURITY DEFINER AS $$
declare
  v_salt varchar;
begin
  if login like '%@%' then
    select users.* into usr from users where u_email = login limit 1;
  else
    select users.* into usr from users where u_login = login limit 1;
  end if;

  if usr is null then
    raise exception 'ACCOUNT_NOT_FOUND' using errcode = '28000';
  end if;

  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  if usr.u_pass != encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex') then
    raise exception 'INVALID_PASSWORD' using errcode = '28P01';
  end if;

  perform set_config('jwt.claims.user_id', usr.u_id::text, true);
  insert into session (ss_id, ss_user, ss_lifetime) values (gen_random_uuid(), usr.u_id, 86400)
  returning * into sess;
end;
$$;
GRANT ALL ON FUNCTION public.login(character varying, character varying, OUT usr public.users) TO anonymous;
