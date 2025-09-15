--! Previous: sha1:d968275009d1e14f194c45ca386b4607be28e1e8
--! Hash: sha1:445a032dc1b1ee41fd22f683db4f210c9933fef5

--! split: 1-current.sql
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

drop function if exists upsert_event(info event, instances event_instance[], trainers event_trainer[], cohorts event_target_cohort[], registrations event_registration[]);
drop function if exists upsert_event(info event_type_input, instances event_instance_type_input[], trainers event_trainer_type_input[], cohorts event_target_cohort_type_input[], registrations event_registration_type_input[]);

drop type if exists event_type_input;
drop type if exists event_instance_type_input;
drop type if exists event_instance_trainer_type_input;
drop type if exists event_trainer_type_input;
drop type if exists event_target_cohort_type_input;
drop type if exists event_registration_type_input;

CREATE TYPE public.event_type_input AS (
  id bigint,
  name text,
  summary text,
  description text,
  description_member text,
  type public.event_type,
  location_id bigint,
  location_text text,
  capacity integer,
  is_visible boolean,
  is_public boolean,
  is_locked boolean,
  enable_notes boolean,
  payment_type event_payment_type,
  member_price public.price,
  guest_price public.price
);

CREATE TYPE public.event_instance_trainer_type_input AS (
  id bigint,
  person_id bigint
);

CREATE TYPE public.event_instance_type_input AS (
  id bigint,
  since timestamp with time zone,
  until timestamp with time zone,
  is_cancelled boolean,
  trainers event_instance_trainer_type_input[]
);

CREATE TYPE public.event_trainer_type_input AS (
  id bigint,
  person_id bigint,
  lessons_offered integer
);

CREATE TYPE public.event_target_cohort_type_input AS (
  id bigint,
  cohort_id bigint
);

CREATE TYPE public.event_registration_type_input AS (
  id bigint,
  person_id bigint,
  couple_id bigint
);

CREATE OR REPLACE FUNCTION upsert_event(
  info event_type_input,
  instances event_instance_type_input[],
  trainers event_trainer_type_input[],
  cohorts event_target_cohort_type_input[],
  registrations event_registration_type_input[]
) RETURNS event LANGUAGE plpgsql AS $$
declare
  instance event_instance_type_input;
  trainer event_trainer_type_input;
  instance_trainer event_instance_trainer_type_input;
  cohort event_target_cohort_type_input;
  registration event_registration_type_input;
  v_event event;
  v_instance event_instance;
begin
  if info.id is not null then
    update event set
      name=info.name,
      summary=info.summary,
      description=info.description,
      description_member=info.description_member,
      type=info.type,
      location_id=info.location_id,
      location_text=info.location_text,
      capacity=info.capacity,
      is_visible=info.is_visible,
      is_public=info.is_public,
      is_locked=info.is_locked,
      enable_notes=info.enable_notes,
      payment_type=info.payment_type,
      guest_price=info.guest_price,
      member_price=info.member_price
    where id=info.id
    returning * into v_event;
  else
    insert into event (
      name,
      summary,
      description,
      description_member,
      type,
      location_id,
      location_text,
      capacity,
      is_visible,
      is_public,
      is_locked,
      enable_notes,
      payment_type,
      guest_price,
      member_price
    )
    values (
      info.name,
      info.summary,
      info.description,
      info.description_member,
      info.type,
      info.location_id,
      info.location_text,
      info.capacity,
      info.is_visible,
      info.is_public,
      info.is_locked,
      info.enable_notes,
      info.payment_type,
      info.guest_price,
      info.member_price
    )
    returning * into v_event;
  end if;

  foreach instance in array instances loop
    if instance.id is not null then
      if instance.since is null and instance.until is null then
        delete from event_instance where id=instance.id;
        v_instance.id := null;
      else
        update event_instance
        set since=instance.since, until=instance.until, is_cancelled=instance.is_cancelled
        where id=instance.id
        returning * into v_instance;
      end if;
    else
      insert into event_instance (event_id, since, until, is_cancelled)
      values (v_event.id, instance.since, instance.until, instance.is_cancelled)
      returning * into v_instance;
    end if;

    if v_instance.id is not null then
      foreach instance_trainer in array instance.trainers loop
        if instance_trainer.id is not null then
          if instance_trainer.person_id is null then
            delete from event_instance_trainer where id=instance_trainer.id;
          end if;
        else
          insert into event_instance_trainer (instance_id, person_id)
          values (v_instance.id, instance_trainer.person_id);
        end if;
      end loop;
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
      insert into event_trainer (event_id, person_id, lessons_offered)
      values (v_event.id, trainer.person_id, coalesce(trainer.lessons_offered, 0));
    end if;
  end loop;

  foreach cohort in array cohorts loop
    if cohort.id is not null then
      if cohort.cohort_id is null then
        delete from event_target_cohort where id=cohort.id;
      end if;
    else
      insert into event_target_cohort (event_id, cohort_id)
      values (v_event.id, cohort.cohort_id);
    end if;
  end loop;

  foreach registration in array registrations loop
    if registration.id is not null then
      if registration.person_id is null and registration.couple_id is null then
        delete from event_registration where id=registration.id;
      else
        update event_registration
        set person_id=registration.person_id, couple_id=registration.couple_id
        where id=registration.id;
      end if;
    else
      insert into event_registration (event_id, person_id, couple_id)
      values (v_event.id, registration.person_id, registration.couple_id);
    end if;
  end loop;

  return v_event;
end;
$$;

select verify_function('public.upsert_event');

COMMENT ON FUNCTION public.upsert_event is null;
GRANT ALL ON FUNCTION public.upsert_event TO anonymous;

drop function if exists public.event_instance_approx_price;

create or replace function public.event_instance_approx_price(v_instance event_instance) returns table (amount numeric(19, 4), currency text) language plpgsql stable as $$
declare
  num_participants bigint;
  duration numeric;
begin
  num_participants := (select count(*) from event join lateral event_registrants(event.*) on true where event.id=v_instance.event_id);
  duration = extract(epoch from (v_instance.until - v_instance.since)) / 60;

  if exists (select 1 from event_instance_trainer where instance_id = v_instance.id) then
    return query
    select
      coalesce(sum((tenant_trainer.member_price_45min).amount * duration / 45 / num_participants)::numeric(19,4), 'NaN') as amount,
      coalesce((tenant_trainer.member_price_45min).currency, 'CZK') as currency
    from event_instance_trainer join tenant_trainer on event_instance_trainer.person_id=tenant_trainer.person_id
    where active and event_instance_trainer.instance_id=v_instance.id and tenant_trainer.tenant_id = event_instance_trainer.tenant_id
    group by (tenant_trainer.member_price_45min).currency;
  else
    return query
    select
      coalesce(sum((tenant_trainer.member_price_45min).amount * duration / 45 / num_participants)::numeric(19,4), 'NaN') as amount,
      coalesce((tenant_trainer.member_price_45min).currency, 'CZK') as currency
    from event_trainer join tenant_trainer on event_trainer.person_id=tenant_trainer.person_id
    where active and event_trainer.event_id=v_instance.event_id and tenant_trainer.tenant_id = event_trainer.tenant_id
    group by (tenant_trainer.member_price_45min).currency;
  end if;
end;
$$;

select verify_function('event_instance_approx_price');

grant all on function event_instance_approx_price to anonymous;

COMMENT ON FUNCTION event_instance_approx_price IS '@simpleCollections only';




CREATE or replace FUNCTION invitation_name(token uuid) RETURNS text LANGUAGE sql STABLE SECURITY DEFINER AS $$
  select person_name(person.*)
  from person_invitation join person on person.id=person_id
  where access_token=token and used_at is null;
$$;
GRANT ALL ON FUNCTION invitation_name TO anonymous;

do $$
begin
  if exists (SELECT 1 fROM pg_type JOIN pg_enum ON pg_type.oid = pg_enum.enumtypid WHERE typname = 'attendance_type' and enumlabel = 'excused') then
    drop view if exists scoreboard;
    drop function if exists event_instance_attendance_summary;
    drop function if exists update_event_attendance;
    update event_attendance set status = 'not-excused' where status = 'excused';

    ALTER TYPE attendance_type RENAME TO attendance_type_old;
    CREATE TYPE attendance_type AS ENUM(
      'unknown',
      'attended',
      'not-excused',
      'cancelled'
    );
    ALTER TABLE event_attendance ALTER COLUMN status DROP DEFAULT;
    ALTER TABLE event_attendance ALTER COLUMN status TYPE attendance_type USING status::text::attendance_type;
    ALTER TABLE event_attendance ALTER COLUMN status SET DEFAULT 'unknown'::attendance_type;
    DROP TYPE attendance_type_old;
  end if;
end;
$$;

CREATE OR REPLACE VIEW public.scoreboard AS
 WITH members AS (
         SELECT person.id
           FROM (public.person
             JOIN public.cohort_membership ON ((cohort_membership.person_id = person.id)))
          WHERE (cohort_membership.active AND (cohort_membership.tenant_id = public.current_tenant_id()))
        ), attendances AS (
         SELECT event_attendance.person_id,
                CASE
                    WHEN (event.type = 'lesson'::public.event_type) THEN 1
                    ELSE 0
                END AS lesson_score,
                CASE
                    WHEN (event.type = 'group'::public.event_type) THEN floor(((EXTRACT(epoch FROM (i.until - i.since)) / (60)::numeric) / (45)::numeric))
                    ELSE (0)::numeric
                END AS group_score,
                CASE
                    WHEN (event.type = 'camp'::public.event_type) THEN (3 + (2 * ((EXTRACT(epoch FROM (i.until - i.since)) > (86400)::numeric))::integer))
                    ELSE 0
                END AS event_score,
            i.since
           FROM (((public.event_attendance
             JOIN public.event_registration ON ((event_registration.id = event_attendance.registration_id)))
             JOIN public.event ON ((event.id = event_registration.event_id)))
             JOIN public.event_instance i ON ((event_attendance.instance_id = i.id)))
          WHERE (((event_attendance.status = 'attended'::public.attendance_type) OR (event.type = 'lesson'::public.event_type)) AND (event.type <> 'reservation'::public.event_type) AND (NOT i.is_cancelled) AND (i.since > '2023-09-01 00:00:00+00'::timestamp with time zone) AND (i.until < date_trunc('day'::text, now())) AND (event_attendance.person_id IN ( SELECT members.id
                   FROM members)))
        ), per_day AS (
         SELECT attendances.person_id,
            LEAST(sum(attendances.lesson_score), (4)::bigint) AS lesson_score,
            sum(attendances.group_score) AS group_score,
            sum(attendances.event_score) AS event_score,
            (((LEAST(sum(attendances.lesson_score), (4)::bigint))::numeric + sum(attendances.group_score)) + (sum(attendances.event_score))::numeric) AS total_score,
            attendances.since
           FROM attendances
          GROUP BY attendances.person_id, attendances.since
        )
 SELECT person_id,
    (sum(lesson_score))::bigint AS lesson_total_score,
    (sum(group_score))::bigint AS group_total_score,
    (sum(event_score))::bigint AS event_total_score,
    (sum((((lesson_score)::numeric + group_score) + (event_score)::numeric)))::bigint AS total_score,
    rank() OVER (ORDER BY (sum((((lesson_score)::numeric + group_score) + (event_score)::numeric)))::bigint DESC) AS ranking
   FROM per_day
  GROUP BY person_id
  ORDER BY (sum((((lesson_score)::numeric + group_score) + (event_score)::numeric)))::bigint DESC;

COMMENT ON VIEW public.scoreboard IS '@foreignKey (person_id) references person (id)
@simpleCollections only';

GRANT ALL ON TABLE public.scoreboard TO anonymous;

CREATE or replace FUNCTION event_instance_attendance_summary(e event_instance)
    RETURNS TABLE(status attendance_type, count integer)
    LANGUAGE sql STABLE
    AS $$
  select status, count(status) as count from event_attendance where instance_id=e.id group by status;
$$;

COMMENT ON FUNCTION event_instance_attendance_summary(e event_instance) IS '@simpleCollections only';

GRANT ALL ON FUNCTION event_instance_attendance_summary(e event_instance) TO anonymous;

CREATE or replace FUNCTION update_event_attendance(instance_id bigint, person_id bigint, status attendance_type, note text) RETURNS event_attendance
    LANGUAGE plpgsql
    AS $$
declare
  att event_attendance;
  reg event_registration;
begin
  select event_registration.* into reg
  from event_registration
  join event_instance on event_registration.event_id=event_instance.event_id
  left join couple on couple_id=couple.id
  where event_instance.id=$1 and $2 in (event_registration.person_id, man_id, woman_id);

  insert into event_attendance (registration_id, instance_id, person_id, status, note)
  values (reg.id, $1, $2, $3, $4)
  on conflict on constraint event_attendance_unique_event_person_key do update set status=$3, note=$4
  returning * into att;
  return att;
end
$$;

select verify_function('update_event_attendance');

GRANT ALL ON FUNCTION update_event_attendance(instance_id bigint, person_id bigint, status attendance_type, note text) TO anonymous;
