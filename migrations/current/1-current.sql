-- Write your migration here

alter type jwt_token
  alter attribute my_person_ids type json,
  alter attribute my_tenant_ids type json,
  alter attribute my_cohort_ids type json,
  alter attribute my_couple_ids type json;

CREATE or replace FUNCTION app_private.create_jwt_token(u public.users) RETURNS public.jwt_token LANGUAGE sql STABLE AS $$
  with details as (
    SELECT
      user_id,
      user_proxy.person_id as person_id,
      tenant_memberships || tenant_trainers || tenant_administrators as my_tenant_ids,
      cohort_memberships as my_cohort_ids,
      couple_ids as my_couple_ids,
      current_tenant_id() = ANY (tenant_memberships || tenant_trainers || tenant_administrators) as is_member,
      current_tenant_id() = ANY (tenant_trainers) as is_trainer,
      current_tenant_id() = ANY (tenant_administrators) as is_admin
    from user_proxy
    join auth_details on user_proxy.person_id=auth_details.person_id
    where user_id=u.u_id
  ) select
    extract(epoch from now() + interval '7 days')::integer,
    u.u_id,
    current_tenant_id(),
    u.u_login,
    u.u_email,
    array_to_json(array_agg(person_id)) as my_person_ids,
    array_to_json(array_accum(my_tenant_ids)) as my_tenant_ids,
    array_to_json(array_accum(my_cohort_ids)) as my_cohort_ids,
    array_to_json(array_accum(my_couple_ids)) as my_couple_ids,
    bool_or(is_member) as is_member,
    bool_or(is_trainer) as is_trainer,
    bool_or(is_admin) as is_admin
  from details group by user_id;
$$;

CREATE or replace FUNCTION public.refresh_jwt() RETURNS public.jwt_token LANGUAGE sql STABLE SECURITY DEFINER AS $$
  SELECT app_private.create_jwt_token(users) FROM users WHERE u_id = nullif(current_setting('jwt.claims.user_id', true), '')::integer;
$$;
GRANT ALL ON FUNCTION public.refresh_jwt() TO anonymous;

CREATE or replace FUNCTION public.login(login character varying, passwd character varying, OUT usr public.users, OUT jwt public.jwt_token) RETURNS record
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  v_salt varchar;
begin
  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  login := trim(login);
  select users.* into usr from users where lower(u_login) = lower(login) and u_pass = encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex') limit 1;
  if usr is null then
    select users.* into usr from users where lower(u_email) = lower(login) and u_pass = encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex') limit 1;
  end if;

  if usr is null then
    raise exception 'INVALID_CREDENTIALS' using errcode = '28P01';
  end if;

  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);
  update users set last_login = now() where id = usr.id;
end;
$$;

CREATE or replace FUNCTION public.register_using_invitation(email text, login text, passwd text, token uuid, OUT usr public.users, OUT jwt public.jwt_token) RETURNS record
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

CREATE or replace FUNCTION app_private.tg_users__trim_login() RETURNS trigger LANGUAGE plpgsql AS $$
declare
  v_salt varchar;
begin
  NEW.u_login := trim(NEW.u_login);
  return NEW;
end;
$$;
drop TRIGGER if exists _300_trim_login ON public.users;
CREATE TRIGGER _300_trim_login BEFORE INSERT OR UPDATE ON public.users FOR EACH ROW EXECUTE FUNCTION app_private.tg_users__trim_login();

drop INDEX if exists idx_23964_u_login;
drop index if exists idx_24786_u_login;
drop index if exists users_login_key;
drop index if exists users_email_key;
create unique index users_login_key on users (u_login) where u_id not in (1050,533,882,1075,489,82,1138,689,45,433,13,142,223,1046,498,1106,105,130);
create unique index users_email_key on users (u_email) where u_id not in (916,914,915,587,765,696,786,259,306,540,443,1042,585,825,413,428,985,935,218,581,827,826,165,207,232,945,990,1039,1040,606,607,896,975,496,511,898,920,970,724,725,958,542,543,886,223,348,23,973,128,988,517,978,928,930,968,939,951,950,808,723,557,1013,1014,1015,820,841,599,681,31,40,120,360,417,419,545,39,643,670,782,790,668,894,922,925,803,812,153,602,198,239,397,686,846,537,893,974,993,755,805,337,155,629,630,554,994,661,891,434,436,640,829,683,505,1,648,649,677,4,162,17,565,700,701,952,999,1003,1006,346,576,986,582,315,753,76,93,316,359,370,508,506,509,510,754,811,430,654,598,612,698,923,943,971,679,798,799);

alter table users alter column u_login type citext;
alter table users alter column u_email type citext;

drop view if exists scoreboard;
create or replace view scoreboard as
  with members as (
    select person.id
    from person
    inner join cohort_membership on cohort_membership.person_id=person.id
    where now() <@ cohort_membership.active_range and tenant_id=current_tenant_id()
  ), attendances as (
    select
      event_attendance.person_id,
      case when event.type = 'lesson' then 1 else 0 end as lesson_score,
      case when event.type = 'group' then floor(extract(epoch from i.until - i.since) / 60 / 45) else 0 end as group_score,
      case when event.type = 'camp' then 3 + 2 * (extract(epoch from i.until - i.since) > 86400)::int else 0 end as event_score,
      i.since
    from event_attendance
    inner join event_registration on event_registration.id=event_attendance.registration_id
    inner join event on event.id=event_registration.event_id
    inner join event_instance i on event_attendance.instance_id=i.id
    where (event_attendance.status = 'attended' or event.type = 'lesson')
    and event.type <> 'reservation'
    and i.since > '2023-09-01T00:00:00.0000Z'
    and i.until < date_trunc('day', now())
    and event_attendance.person_id in (select id from members)
  ), per_day as (
    select
      person_id,
      least(SUM(lesson_score), 4) AS lesson_score,
      SUM(group_score) AS group_score,
      SUM(event_score) AS event_score,
      least(SUM(lesson_score), 4) + sum(group_score) + sum(event_score) AS total_score,
      since
    from attendances
    group by person_id, since
  )
  select
    person_id,
    SUM(lesson_score)::bigint AS lesson_total_score,
    SUM(group_score)::bigint AS group_total_score,
    SUM(event_score)::bigint AS event_total_score,
    SUM(lesson_score + group_score + event_score)::bigint AS total_score,
    rank() OVER (ORDER BY SUM(lesson_score + group_score) DESC) AS ranking
  from per_day
  group by person_id
  ORDER BY total_score DESC;
comment on view scoreboard is E'@foreignKey (person_id) references person (id)
@simpleCollections only';
grant all on scoreboard to anonymous;


CREATE or replace FUNCTION public.upsert_event(
  INOUT info public.event,
  instances public.event_instance[],
  trainers public.event_trainer[],
  cohorts public.event_target_cohort[],
  registrations public.event_registration[]
) RETURNS public.event LANGUAGE plpgsql AS $$
declare
  instance event_instance;
  trainer event_trainer;
  cohort event_target_cohort;
  registration event_registration;
begin
  if info.id is not null then
    update event set name=info.name, summary=info.summary, description=info.description, type=info.type, location_text=info.location_text, capacity=info.capacity, is_visible=info.is_visible, is_public=info.is_public, is_locked=info.is_locked, enable_notes=info.enable_notes where id=info.id;
  else
    insert into event (name, summary, description, type, location_text, capacity, is_visible, is_public, is_locked, enable_notes)
    values (info.name, info.summary, info.description, info.type, info.location_text, info.capacity, info.is_visible, info.is_public, info.is_locked, info.enable_notes)
    returning * into info;
  end if;

  foreach instance in array instances loop
    if instance.id is not null then
      if instance.since is null and instance.until is null then
        delete from event_instance where id=instance.id;
      else
        update event_instance set since=instance.since, until=instance.until where id=instance.id;
      end if;
    else
      insert into event_instance (event_id, since, until) values (info.id, instance.since, instance.until);
    end if;
  end loop;

  foreach trainer in array trainers loop
    if trainer.id is not null then
      if trainer.person_id is null then
        delete from event_trainer where id=trainer.id;
      else
        update event_trainer set lessons_offered=trainer.lessons_offered where id=trainer.id;
      end if;
    else
      insert into event_trainer (event_id, person_id, lessons_offered) values (info.id, trainer.person_id, coalesce(trainer.lessons_offered, 0));
    end if;
  end loop;

  foreach cohort in array cohorts loop
    if cohort.id is not null then
      if cohort.cohort_id is null then
        delete from event_target_cohort where id=cohort.id;
      end if;
    else
      insert into event_target_cohort (event_id, cohort_id) values (info.id, cohort.cohort_id);
    end if;
  end loop;

  foreach registration in array registrations loop
    if registration.id is not null then
      if registration.person_id is null and registration.couple_id is null then
        delete from event_registration where id=registration.id;
      else
        update event_registration
        set person_id=registration.person_id, couple_id=registration.couple_id, is_confirmed=registration.is_confirmed
        where id=registration.id;
      end if;
    else
      insert into event_registration (event_id, person_id, couple_id, is_confirmed)
      values (info.id, registration.person_id, registration.couple_id, registration.is_confirmed);
    end if;
  end loop;
end;
$$;
select verify_function('upsert_event');
COMMENT ON FUNCTION public.upsert_event(INOUT info public.event, instances public.event_instance[], trainers public.event_trainer[], cohorts public.event_target_cohort[], registrations public.event_registration[]) IS '@arg0variant patch
@arg1variant patch
@arg2variant patch
@arg3variant patch
@arg4variant patch
';
GRANT ALL ON FUNCTION public.upsert_event(INOUT info public.event, instances public.event_instance[], trainers public.event_trainer[], cohorts public.event_target_cohort[], registrations public.event_registration[]) TO anonymous;

ALTER TABLE event ALTER COLUMN id SET GENERATED BY DEFAULT;
ALTER TABLE event_instance ALTER COLUMN id SET GENERATED BY DEFAULT;
ALTER TABLE event_trainer ALTER COLUMN id SET GENERATED BY DEFAULT;
ALTER TABLE event_registration ALTER COLUMN id SET GENERATED BY DEFAULT;
ALTER TABLE event_target_cohort ALTER COLUMN id SET GENERATED BY DEFAULT;


drop type if exists register_to_event_type cascade;
do $$ begin
  if not exists (select 1 from pg_type where typname = 'register_to_event_type' and typcategory = 'C') then
    create type register_to_event_type as (
      event_id bigint,
      person_id bigint,
      couple_id bigint,
      note text,
      lessons event_lesson_demand[]
    );
  end if;
end $$;

create or replace function register_to_event_many(registrations register_to_event_type[]) returns setof event_registration language plpgsql strict as $$
declare
  event event;
  created_ids bigint[] := array[]::bigint[];
  registration register_to_event_type;
  created event_registration;
  demand event_lesson_demand;
begin
  foreach registration in array registrations loop
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

    insert into event_registration (event_id, person_id, couple_id, note)
    values (registration.event_id, registration.person_id, registration.couple_id, registration.note)
    returning * into created;
    created_ids := created_ids || created.id;
    foreach demand in array registration.lessons loop
      perform set_lesson_demand(created.id, demand.trainer_id, demand.lesson_count);
    end loop;
  end loop;
  return query select * from event_registration where id = any (created_ids);
end;
$$;
comment on column event_lesson_demand.registration_id is E'@hasDefault';
select verify_function('public.register_to_event_many');
GRANT ALL ON FUNCTION public.register_to_event_many TO anonymous;


CREATE OR REPLACE FUNCTION public.my_tenants_array() RETURNS bigint[] LANGUAGE sql STABLE AS $$
  SELECT translate(nullif(current_setting('jwt.claims.my_tenant_ids', true), ''), '[]', '{}')::bigint[];
$$;
COMMENT ON FUNCTION public.my_tenants_array() IS '@omit';
GRANT ALL ON FUNCTION public.my_tenants_array() TO anonymous;

CREATE OR REPLACE FUNCTION public.my_cohorts_array() RETURNS bigint[] LANGUAGE sql STABLE AS $$
  SELECT translate(nullif(current_setting('jwt.claims.my_cohort_ids', true), ''), '[]', '{}')::bigint[];
$$;
COMMENT ON FUNCTION public.my_cohorts_array() IS '@omit';
GRANT ALL ON FUNCTION public.my_cohorts_array() TO anonymous;

CREATE OR REPLACE FUNCTION public.my_couples_array() RETURNS bigint[] LANGUAGE sql STABLE AS $$
  SELECT translate(nullif(current_setting('jwt.claims.my_couple_ids', true), ''), '[]', '{}')::bigint[];
$$;
COMMENT ON FUNCTION public.my_couples_array() IS '@omit';
GRANT ALL ON FUNCTION public.my_couples_array() TO anonymous;

CREATE OR REPLACE FUNCTION public.my_persons_array() RETURNS bigint[] LANGUAGE sql STABLE AS $$
  SELECT translate(nullif(current_setting('jwt.claims.my_person_ids', true), ''), '[]', '{}')::bigint[];
$$;
COMMENT ON FUNCTION public.my_persons_array() IS '@omit';
GRANT ALL ON FUNCTION public.my_persons_array() TO anonymous;



DO $do$ BEGIN
  IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname='anonymous') THEN
    CREATE ROLE anonymous;
  END IF;
  IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname='member') THEN
    CREATE ROLE member;
  END IF;
  IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname='trainer') THEN
    CREATE ROLE trainer;
  END IF;
  IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname='administrator') THEN
    CREATE ROLE administrator;
  END IF;
END $do$;

grant anonymous to member, trainer, administrator;
grant member to trainer, administrator;
grant trainer to administrator;


select app_private.drop_policies('public.event_attendance');
CREATE POLICY admin_all ON public.event_attendance TO administrator USING (true);
CREATE POLICY admin_trainer ON public.event_attendance TO trainer USING (exists (
  select 1
  from event_instance
  left join event_trainer on event_instance.event_id=event_trainer.event_id
  left join event_instance_trainer on event_instance.id=event_instance_trainer.instance_id
  where event_attendance.instance_id=event_instance.id and (
    event_instance_trainer.person_id = any (my_persons_array())
    or event_trainer.person_id = any (my_persons_array())
  )
));
CREATE POLICY view_visible_event ON public.event_attendance FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event_instance
  WHERE (event_attendance.instance_id = event_instance.id))));

select app_private.drop_policies('public.event_instance');
CREATE POLICY admin_all ON public.event_instance TO administrator USING (true);
CREATE POLICY view_visible_event ON public.event_instance FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event
  WHERE (event_instance.event_id = event.id))));

select app_private.drop_policies('public.event');
CREATE POLICY admin_same_tenant ON public.event to administrator USING (tenant_id = any (my_tenants_array()));
CREATE POLICY view_public ON public.event FOR SELECT TO anonymous USING (is_public = true or tenant_id = any (my_tenants_array()));
CREATE POLICY my_tenant ON public.event AS RESTRICTIVE USING (tenant_id = current_tenant_id());


DROP TABLE IF EXISTS tenant_location CASCADE;
CREATE TABLE tenant_location (
    id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    name text NOT NULL,
    description jsonb NOT NULL,
    address public.address_domain,
    tenant_id bigint not null references tenant,
    created_at timestamptz not null default now(),
    updated_at timestamptz not null default now()
);
grant all on tenant_location to anonymous;
alter table tenant_location enable row level security;

comment on table tenant_location is '@simpleCollections only';

select app_private.drop_policies('public.tenant_location');
create policy admin_all on tenant_location to administrator using (true) with check (true);
create policy public_view on tenant_location for select to anonymous;
CREATE POLICY my_tenant ON tenant_location AS RESTRICTIVE USING (tenant_id = current_tenant_id());

CREATE INDEX ON "public"."tenant_location"("tenant_id");

alter table event add column if not exists location_id bigint null references tenant_location (id);
alter table event_instance add column if not exists location_id bigint null references tenant_location (id);
