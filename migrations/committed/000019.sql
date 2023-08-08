--! Previous: sha1:29ccef0ffa32dc2dae6f9d6e023c01f57337a0e2
--! Hash: sha1:1b19dc580423b5e0017cd48b9223122b942407c9

--! split: 1-cleanup.sql
alter table if exists public.pary_navrh set schema app_private;
alter table if exists public.parameters set schema app_private;

COMMENT ON TABLE public.attendee_external IS E'@omit';
COMMENT ON TABLE public.attendee_user IS E'@omit';
COMMENT ON TABLE public.rozpis IS E'@omit';
COMMENT ON TABLE public.rozpis_item IS E'@omit';
COMMENT ON TABLE public.nabidka IS E'@omit';
COMMENT ON TABLE public.nabidka_item IS E'@omit';
comment on table couple is E'@omit update,delete';
comment on table cohort_membership is E'@omit create,update,delete
@simpleCollections only';
comment on table tenant_membership is E'@omit create,update,delete
@simpleCollections only';
comment on table tenant_administrator is E'@omit create,update,delete
@simpleCollections only';
comment on table tenant_trainer is E'@omit create,update,delete
@simpleCollections only';
comment on table session is E'@omit';
comment on table room_attachment is E'@omit update,order,filter';
comment on table tenant_attachment is E'@omit update,order,filter';
comment on table permissions is E'@omit insert,update,delete,order,filter\n@simpleCollections only';
comment on function verify_function is E'@omit';
comment on table address is E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.person_address IS E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.person_phone IS E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.person_email IS E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.event_attendance IS E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.event_instance IS E'@omit create,update,delete
@simpleCollections both';
COMMENT ON TABLE public.event_instance_trainer IS E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.event_lesson_demand IS E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.event_target_cohort IS E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.event_registration IS E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.event_trainer IS E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.platby_raw IS E'@omit create,update,delete';
COMMENT ON TABLE public.platby_group_skupina IS E'@omit create,update,delete';
COMMENT ON TABLE public.tenant IS E'@omit create,delete
@simpleCollections only';
COMMENT ON TABLE public.user_proxy IS E'@omit create,update,delete
@simpleCollections only';

create index if not exists couple_active_idx on couple (active);
create index if not exists tenant_membership_active_idx on tenant_membership (active);
create index if not exists cohort_membership_active_idx on cohort_membership (active);
create index if not exists tenant_administrator_active_idx on tenant_administrator (active);
create index if not exists tenant_trainer_active_idx on tenant_trainer (active);

alter table cohort_group alter column description set default '';
alter table event alter column summary set default '';
alter table skupiny alter column internal_info set default '';

do $$ begin
  if not exists (select 1 from information_schema.columns where table_name = 'tenant_administrator' and column_name = 'id') then
    ALTER TABLE tenant_administrator DROP CONSTRAINT tenant_administrator_pkey;
    ALTER TABLE tenant_administrator ADD COLUMN id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY;
  end if;
end $$;
do $$ begin
  if not exists (select 1 from information_schema.columns where table_name = 'tenant_trainer' and column_name = 'id') then
    ALTER TABLE tenant_trainer DROP CONSTRAINT tenant_trainer_pkey;
    ALTER TABLE tenant_trainer ADD COLUMN id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY;
  end if;
end $$;
do $$ begin
  if not exists (select 1 from information_schema.columns where table_name = 'tenant_membership' and column_name = 'id') then
    ALTER TABLE tenant_membership DROP CONSTRAINT tenant_membership_pkey;
    ALTER TABLE tenant_membership ADD COLUMN id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY;
  end if;
end $$;
do $$ begin
  if not exists (select 1 from information_schema.columns where table_name = 'cohort_membership' and column_name = 'id') then
    ALTER TABLE cohort_membership DROP CONSTRAINT cohort_membership_pkey;
    ALTER TABLE cohort_membership ADD COLUMN id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY;
  end if;
end $$;
do $$ begin
  if not exists (select 1 from information_schema.columns where table_name = 'user_proxy' and column_name = 'id') then
    ALTER TABLE user_proxy DROP CONSTRAINT user_proxy_pkey;
    ALTER TABLE user_proxy ADD COLUMN id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY;
  end if;
end $$;

drop table if exists tenant_person;
drop function if exists fix_unpaired_couples;
drop function if exists prospect_form_dancer;
drop function if exists legacy_duplicate_rozpis;
drop function if exists legacy_duplicate_nabidka;
drop function if exists current_permissions;
drop function if exists insert_revision;
drop function if exists event_free_slots;
drop function if exists nabidka_free_lessons;
drop function if exists nabidka_my_lessons;
drop function if exists reservation_set_desired_lessons;
drop function if exists event_remaining_spots;
drop function if exists book_lesson;
drop function if exists cancel_lesson;
drop function if exists cancel_participation;
drop function if exists create_participation;
drop function if exists create_participation_external;
drop function if exists schedules_for_range;
drop function if exists reservations_for_range;
drop function if exists active_couples;
drop function if exists create_couple;

create or replace function regenerate_event_registration_from_attendees() returns void language plpgsql security definer as $$
begin
  insert into event_registration (tenant_id, event_id, person_id, note, is_confirmed)
  select
    attendee_user.tenant_id,
    attendee_user.event_id,
    (select id from person where legacy_user_id = attendee_user.user_id),
    case attendee_user.notes when '' then null else attendee_user.notes end,
    true
  from attendee_user;
end;
$$;
select verify_function('regenerate_event_registration_from_attendees');

--! split: 2-rls-support.sql
CREATE or replace FUNCTION my_person_ids() RETURNS SETOF bigint LANGUAGE sql STABLE AS $$
  select person.id
  from person join user_proxy on person_id=person.id
  where user_id = current_user_id();
$$;
GRANT ALL ON FUNCTION my_person_ids() TO anonymous;
comment on function my_person_ids is '@omit';

CREATE or replace FUNCTION my_couple_ids() RETURNS SETOF bigint LANGUAGE sql STABLE AS $$
  select couple.id
  from couple join person on (man_id = person.id or woman_id = person.id) join user_proxy on person_id=person.id
  where user_id = current_user_id();
$$;
GRANT ALL ON FUNCTION my_couple_ids() TO anonymous;
comment on function my_couple_ids is '@omit';

CREATE or replace FUNCTION my_tenant_ids() RETURNS SETOF bigint LANGUAGE sql STABLE AS $$
  select tenant.id
  from tenant join tenant_membership on tenant.id = tenant_id
  where tenant_membership.active = true and person_id in (select my_person_ids());
$$;
GRANT ALL ON FUNCTION my_tenant_ids() TO anonymous;
comment on function my_tenant_ids is '@omit';

create or replace FUNCTION current_couple_ids() RETURNS SETOF bigint LANGUAGE sql STABLE AS $$
  select my_couple_ids();
$$;
GRANT ALL ON FUNCTION public.current_couple_ids() TO anonymous;

--! split: 3-rls.sql
select app_private.drop_policies('public.address');
create policy admin_all on address to administrator using (true);
create policy admin_personal on address using (id in (select address_id from person_address where person_id in (select my_person_ids())));
create policy view_visible_person on address for select using (exists (select 1 from person_address where address_id = id));

select app_private.drop_policies('public.couple');
create policy admin_all on couple to administrator using (true);
create policy view_visible_person on couple for select using (exists (select 1 from person where man_id = person.id or woman_id = person.id));

select app_private.drop_policies('public.user_proxy');
create policy admin_all on user_proxy to administrator using (true);
create policy view_personal on user_proxy for select using (user_id = current_user_id());

select app_private.drop_policies('public.person');
create policy admin_all on person to administrator using (true);
create policy view_same_tenant on person for select using (exists (select 1 from tenant_membership where active=true and person_id = id and tenant_id in (select my_tenant_ids())));
create policy view_tenant_admin on person for select using (exists (select 1 from tenant_administrator where active=true and person_id = id));
create policy view_tenant_trainer on person for select using (exists (select 1 from tenant_trainer where active=true and person_id = id));

select app_private.drop_policies('public.person_address');
create policy admin_all on person_address to administrator using (true);
create policy admin_personal on person_address using (person_id in (select my_person_ids()));
create policy view_visible_person on person_address for select using (exists (select 1 from person where person_id = person.id));

select app_private.drop_policies('public.person_email');
create policy admin_all on person_email to administrator using (true);
create policy admin_personal on person_email using (person_id in (select my_person_ids()));
create policy view_visible_person on person_email for select using (exists (select 1 from person where person_id = person.id));

select app_private.drop_policies('public.person_phone');
create policy admin_all on person_phone to administrator using (true);
create policy admin_personal on person_phone using (person_id in (select my_person_ids()));
create policy view_visible_person on person_phone for select using (exists (select 1 from person where person_id = person.id));

alter table cohort_membership add column if not exists tenant_id bigint not null default current_tenant_id();
CREATE INDEX if not exists "tenant_id" ON cohort_membership (tenant_id);
select app_private.drop_policies('public.cohort_membership');
create policy admin_all on cohort_membership to administrator using (true);
create policy view_visible_person on cohort_membership for select using (exists (select 1 from person where person_id = person.id));

select app_private.drop_policies('public.tenant_membership');
create policy admin_all on tenant_membership to administrator using (true);
create policy view_visible_person on tenant_membership for select using (true);

select app_private.drop_policies('public.tenant_administrator');
create policy admin_all on tenant_administrator to administrator using (true);
create policy public_view on tenant_administrator for select using (true);

select app_private.drop_policies('public.tenant_trainer');
create policy admin_all on tenant_trainer to administrator using (true);
create policy public_view on tenant_trainer for select using (true);

select app_private.drop_policies('public.event_attendance');
create policy admin_all on event_attendance to administrator using (true);
create policy view_visible_event on event_attendance for select using (exists (select 1 from event_instance where instance_id = event_instance.id));

select app_private.drop_policies('public.event_instance');
create policy admin_all on event_instance to administrator using (true);
create policy view_visible_event on event_instance for select using (exists (select 1 from event where event_id = event.id));

select app_private.drop_policies('public.event_instance_trainer');
create policy admin_all on event_instance_trainer to administrator using (true);
create policy view_visible_event on event_instance_trainer for select using (exists (select 1 from event_instance where instance_id = event_instance.id));

select app_private.drop_policies('public.event_lesson_demand');
create policy admin_all on event_lesson_demand to administrator using (true);
create policy view_visible_event on event_lesson_demand for select using (exists (select 1 from event_registration where registration_id = event_registration.id));

select app_private.drop_policies('public.event_registration');
create policy admin_all on event_registration to administrator using (true);
create policy view_visible_event on event_registration for select using (exists (select 1 from event where event_id = event.id));

select app_private.drop_policies('public.event_trainer');
create policy admin_all on event_trainer to administrator using (true);
create policy view_visible_event on event_trainer for select using (exists (select 1 from event where event_id = event.id));

select app_private.drop_policies('public.event_target_cohort');
create policy admin_all on event_target_cohort to administrator using (true);
create policy view_visible_event on event_target_cohort for select using (exists (select 1 from event where event_id = event.id));

select app_private.drop_policies('public.event');
CREATE POLICY admin_all ON event TO administrator USING (true);
create policy view_same_tenant on event for select using (tenant_id in (select my_tenant_ids()));
CREATE POLICY view_public ON event FOR SELECT TO anonymous USING ((is_public = true));

--! split: 4-event.sql
alter table event_registration drop constraint if exists event_registration_unique_event_person_couple_key;
alter table event_registration add constraint event_registration_unique_event_person_couple_key unique (event_id, person_id, couple_id);
alter table event_lesson_demand drop constraint if exists eld_unique_registration_trainer_key;
alter table event_lesson_demand add constraint eld_unique_registration_trainer_key unique (registration_id, trainer_id);
alter table event_trainer add column if not exists lessons_offered int not null default 0;

drop function if exists event_instances_for_range;
CREATE or replace FUNCTION public.event_instances_for_range(search_range tstzrange, only_mine boolean) RETURNS SETOF public.event_instance LANGUAGE sql STABLE AS $$
  select event_instance.* from event_instance join event on event_id=event.id
  where event.is_visible = true and search_range && range
  and case only_mine
    when false then true
    else exists (select 1 from event_registration where event_id=event.id and (person_id in (select my_person_ids()) or couple_id in (select my_couple_ids()))) end
  order by range asc;
$$;
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;
comment on function event_instances_for_range is E'@simpleCollections only';

CREATE or replace FUNCTION public.person_primary_phone(p public.person) RETURNS text LANGUAGE sql STABLE AS $$
  select phone from person_phone where person_id = p.id and is_primary = true;
$$;
CREATE or replace FUNCTION public.person_primary_address(p public.person) RETURNS address LANGUAGE sql STABLE AS $$
  select address.* from address join person_address on address_id=address.id where person_id = p.id and is_primary = true;
$$;
CREATE or replace FUNCTION public.person_primary_email(p public.person) RETURNS text LANGUAGE sql STABLE AS $$
  select email from person_email where person_id = p.id and is_primary = true;
$$;
CREATE or replace FUNCTION public.event_remaining_person_spots(e public.event) RETURNS integer LANGUAGE sql STABLE AS $$
  select e.capacity - (select sum(case when couple_id is not null then 2 else 1 end) from event_registration where event_id = e.id);
$$;
CREATE or replace FUNCTION public.event_remaining_lessons(e public.event) RETURNS integer LANGUAGE sql STABLE AS $$
  select (select sum(lessons_offered) from event_trainer where event_id = e.id) - (select sum(lesson_count) from event_registration join event_lesson_demand on registration_id = event_registration.id where event_id = e.id);
$$;
CREATE or replace FUNCTION public.event_trainer_lessons_remaining(e public.event_trainer) RETURNS integer LANGUAGE sql STABLE AS $$
  select e.lessons_offered - (select sum(lesson_count) from event_lesson_demand where trainer_id = e.id);
$$;

GRANT ALL ON FUNCTION public.event_trainer_lessons_remaining TO anonymous;
GRANT ALL ON FUNCTION public.event_remaining_person_spots TO anonymous;
GRANT ALL ON FUNCTION public.event_remaining_lessons TO anonymous;
GRANT ALL ON FUNCTION public.person_primary_phone TO anonymous;
GRANT ALL ON FUNCTION public.person_primary_email TO anonymous;
GRANT ALL ON FUNCTION public.person_primary_address TO anonymous;

drop function if exists register_to_event;
create or replace function register_to_event(event_id bigint, note text, person_id bigint default null, couple_id bigint default null) returns public.event_registration language plpgsql strict as $$
#variable_conflict use_variable
declare
  event event;
  registration event_registration;
begin
  select * into event from event where id = event_id;
  select * into registration from event_registration er
  where er.event_id = event_id and (er.person_id = person_id or er.couple_id = couple_id);

  if event is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if person_id not in (select my_person_ids()) and couple_id not in (select my_couple_ids()) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  insert into event_registration (event_id, person_id, couple_id, note)
  values (event_id, person_id, couple_id, note) returning * into registration;
  return registration;
end;
$$;
select verify_function('public.register_to_event');
GRANT ALL ON FUNCTION public.register_to_event TO anonymous;

create or replace function cancel_registration(registration_id bigint) returns void language plpgsql strict as $$
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
  if event.is_locked = true or reg.payment_id is not null then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;
  if reg.person_id not in (select my_person_ids()) and reg.couple_id not in (select my_couple_ids()) then
    raise exception 'ACCESS_DENIED' using errcode = '42501';
  end if;

  delete from event_registration where id = reg.id;
end;
$$;
select verify_function('public.cancel_registration');
GRANT ALL ON FUNCTION public.cancel_registration TO anonymous;

drop function if exists public.set_lesson_demand;
CREATE or replace FUNCTION public.set_lesson_demand(
  registration_id bigint,
  trainer_id bigint,
  lesson_count int
) RETURNS public.event_lesson_demand LANGUAGE plpgsql STRICT AS $$
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
    delete from event_lesson_demand eld where registration_id = registration.id and eld.trainer_id = trainer_id;
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

--! split: 5-login.sql
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

  insert into session (ss_id, ss_user, ss_lifetime) values (gen_random_uuid(), usr.u_id, 86400)
  returning * into sess;
end;
$$;
GRANT ALL ON FUNCTION public.login(character varying, character varying, OUT usr public.users) TO anonymous;

--! split: 6-person.sql
CREATE or replace FUNCTION public.person_couples(p person) RETURNS SETOF public.couple LANGUAGE sql STABLE AS $$
  select couple.* from couple where man_id = p.id or woman_id = p.id;
$$;
GRANT ALL ON FUNCTION public.person_couples(person) TO anonymous;
comment on function person_couples is E'@simpleCollections only';

CREATE or replace FUNCTION public.person_has_user(p person) RETURNS boolean LANGUAGE sql STABLE AS $$
  select exists (select 1 from user_proxy where person_id = p.id);
$$;
GRANT ALL ON FUNCTION public.person_has_user(person) TO anonymous;
