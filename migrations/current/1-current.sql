-- Write your migration here

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
  perform set_config('jwt.claims.my_person_ids', array_to_json(jwt.my_person_ids)::text, true);
  perform set_config('jwt.claims.my_tenant_ids', array_to_json(jwt.my_tenant_ids)::text, true);
  perform set_config('jwt.claims.my_cohort_ids', array_to_json(jwt.my_cohort_ids)::text, true);
  perform set_config('jwt.claims.my_couple_ids', array_to_json(jwt.my_couple_ids)::text, true);
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
  perform set_config('jwt.claims.my_person_ids', array_to_json(jwt.my_person_ids)::text, true);
  perform set_config('jwt.claims.my_tenant_ids', array_to_json(jwt.my_tenant_ids)::text, true);
  perform set_config('jwt.claims.my_cohort_ids', array_to_json(jwt.my_cohort_ids)::text, true);
  perform set_config('jwt.claims.my_couple_ids', array_to_json(jwt.my_couple_ids)::text, true);
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

create or replace view scoreboard as
  with members as (
    select person.id from person inner join cohort_membership on cohort_membership.person_id=person.id where now() <@ cohort_membership.active_range
  ), attendances as (
    select
      event_attendance.person_id,
      case when target_cohort_id is null then 3 else 0 end as lesson_score,
      case when target_cohort_id is null then 0 else 2 end as group_score,
      event_instance.since
    from event_attendance
    inner join event_registration on event_registration.id=event_attendance.registration_id
    inner join event on event.id=event_registration.event_id
    inner join event_instance on event_attendance.instance_id=event_instance.id
    where event_attendance.status = 'attended'
    and event_instance.since > '2023-09-01T00:00:00.0000Z'
    and event_attendance.person_id in (select id from members)
  )
  select
    person_id,
    SUM(lesson_score) AS lesson_total_score,
    SUM(group_score) AS group_total_score,
    SUM(lesson_score + group_score) AS total_score,
    rank() OVER (ORDER BY SUM(lesson_score + group_score) DESC) AS ranking
  from attendances
  group by person_id
  ORDER BY total_score DESC;
comment on view scoreboard is E'@foreignKey (person_id) references person (id)
@simpleCollections only';
grant all on scoreboard to anonymous;
