--! Previous: sha1:cf6a239e9abe6635faf1d2ef239507a520d03d72
--! Hash: sha1:9bbb0a39906aa165fc394ab662acc05e906c4d16

--! split: 1-current.sql
delete from event_instance where until = since;
delete from event where not exists (select 1 from event_instance ei where ei.event_id = event.id);

update event_instance set is_cancelled = false where is_cancelled is null;

alter table event_instance
  alter is_cancelled set default false,
  alter is_cancelled set not null,
  drop constraint if exists event_instance_until_gt_since,
  add constraint event_instance_until_gt_since check (until > since);

alter table tenant_administrator alter until drop default, alter until drop not null;
alter table tenant_trainer alter until drop default, alter until drop not null;
alter table tenant_membership alter until drop default, alter until drop not null;
alter table cohort_membership alter until drop default, alter until drop not null;
alter table couple alter until drop default, alter until drop not null;

update tenant_administrator set until = null where until = 'infinity'::timestamptz;
update tenant_trainer set until = null where until = 'infinity'::timestamptz;
update tenant_membership set until = null where until = 'infinity'::timestamptz;
update cohort_membership set until = null where until = 'infinity'::timestamptz;
update couple set until = null where until = 'infinity'::timestamptz;

alter table user_proxy
  alter since drop NOT NULL,
  alter until drop NOT NULL,
  alter until drop DEFAULT;
update user_proxy set since = null where since = '-infinity'::timestamptz;
update user_proxy set until = null where until = 'infinity'::timestamptz;

drop function if exists archived_announcements;
drop function if exists my_announcements(boolean, boolean);
drop function if exists login;

create or replace function app_private.event_instance_trainers_at(
  v_instance event_instance,
  v_at timestamptz
)
  returns setof tenant_trainer
  language sql stable
as $$
select distinct on (tt.tenant_id, tt.person_id) tt.*
from (
  select eit.tenant_id, eit.person_id
  from event_instance_trainer eit
  where eit.instance_id = v_instance.id

  union all

  select et.tenant_id, et.person_id
  from event_trainer et
  where et.event_id = v_instance.event_id
    and not exists (select 1 from event_instance_trainer eit2 where eit2.instance_id = v_instance.id)
) k
join tenant_trainer tt on tt.tenant_id = k.tenant_id and tt.person_id = k.person_id
where tt.active_range @> v_at
order by tt.tenant_id, tt.person_id, lower(tt.active_range) desc;
$$;
grant all on function app_private.event_instance_trainers_at to anonymous;

create or replace function public.event_instance_trainers(v_instance event_instance)
  returns setof tenant_trainer
  language sql stable
as $$
select * from app_private.event_instance_trainers_at(v_instance, v_instance.since);
$$;

grant all on function event_instance_trainers(event_instance) to anonymous;
comment on function event_instance_trainers(event_instance) is '@simpleCollections only';

create or replace function public.event_instance_approx_price(v_instance event_instance)
  returns table (amount numeric(19,4), currency text)
  language sql stable
as $$
  with stats as (
    select
      (select count(*)
       from event e
       join lateral event_registrants(e.*) r on true
       where e.id = v_instance.event_id)::bigint as num_participants,
      extract(epoch from (v_instance.until - v_instance.since)) / 60.0 as duration
  )
  select
    coalesce(sum((tt.member_price_45min).amount * s.duration / 45 / nullif(s.num_participants, 0)), 'NaN') as amount,
    coalesce((tt.member_price_45min).currency, 'CZK') as currency
  from stats s
  join lateral public.event_instance_trainers(v_instance) tt on true
  group by (tt.member_price_45min).currency;
$$;

grant all on function event_instance_approx_price to anonymous;
COMMENT ON FUNCTION event_instance_approx_price IS '@simpleCollections only';

create or replace function sticky_announcements(
  order_by_updated boolean default false
) returns setof announcement
language sql stable
as $$
  with audience_claims as (
    select
      translate(coalesce(nullif(current_setting('jwt.claims.my_cohort_ids', true), ''), '[]'), '[]', '{}')::bigint[] as cohort_ids,
      coalesce(nullif(current_setting('jwt.claims.is_member', true), '')::boolean, false) as is_member,
      coalesce(nullif(current_setting('jwt.claims.is_trainer', true), '')::boolean, false) as is_trainer,
      coalesce(nullif(current_setting('jwt.claims.is_admin', true), '')::boolean, false) as is_admin
  )
  select announcement.*
  from announcement
  cross join audience_claims ac
  where is_visible = true
    and is_sticky = true
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
    and (
      not exists (
        select 1
        from announcement_audience aa_all
        where aa_all.announcement_id = announcement.id
      )
      or exists (
        select 1
        from announcement_audience aa
        where aa.announcement_id = announcement.id
          and (
            (aa.cohort_id is not null and aa.cohort_id = any (ac.cohort_ids))
            or (aa.audience_role = 'member' and ac.is_member)
            or (aa.audience_role = 'trainer' and ac.is_trainer)
            or (aa.audience_role = 'administrator' and ac.is_admin)
          )
      )
    )
  order by
    case when order_by_updated then updated_at else created_at end desc,
    created_at desc;
$$;
grant all on function sticky_announcements(order_by_updated boolean) to anonymous;

create or replace function my_announcements(
  sticky boolean default false,
  archive boolean default false,
  order_by_updated boolean default false
) returns setof announcement
  language sql stable
as $$
with audience_claims as (
  select
    (select array_agg(cohort_id) from current_cohort_membership cm where cm.person_id = any (current_person_ids())) as cohort_ids,
    (exists (select 1 from current_tenant_membership where person_id = any (current_person_ids()))) as is_member,
    (exists (select 1 from current_tenant_trainer where person_id = any (current_person_ids()))) as is_trainer,
    (exists (select 1 from current_tenant_administrator where person_id = any (current_person_ids()))) as is_admin
)
select a.*
from announcement a
cross join audience_claims ac
where a.is_sticky = sticky
  and a.is_visible = case when sticky then true else not archive end
  and (archive or (a.scheduled_since is null or a.scheduled_since <= now()))
  and (archive or (a.scheduled_until is null or a.scheduled_until >= now()))
  and (
    exists (
      select 1
      from announcement_audience aa
      where aa.announcement_id = a.id and (
        (aa.cohort_id is not null and aa.cohort_id = any (ac.cohort_ids))
          or (aa.audience_role = 'member' and ac.is_member)
          or (aa.audience_role = 'trainer' and ac.is_trainer)
          or (aa.audience_role = 'administrator' and ac.is_admin)
      )
    ) or not exists (
      select 1
      from announcement_audience aa_all
      where aa_all.announcement_id = a.id
    )
  )
order by
  case when order_by_updated then a.updated_at else a.created_at end desc,
  a.created_at desc;
$$;

grant all on function my_announcements to anonymous;

do $$ begin
  if not exists (
    select 1 from pg_type t
    join pg_catalog.pg_namespace n on n.oid = t.typnamespace
    where n.nspname = 'public' and typname = 'login_result'
  ) then
    create type login_result as (
      usr users,
      jwt jwt_token
    );
  end if;
end $$;

comment on type login_result is '@name result';

drop function if exists otp_login;

CREATE or replace FUNCTION otp_login(token uuid)
  RETURNS login_result
  LANGUAGE plpgsql STRICT SECURITY DEFINER
  AS $$
declare
  v_token otp_token;
  usr users;
  jwt jwt_token;
begin
  select * into v_token from otp_token where access_token = token and used_at is null and expires_at > now();
  if not found then
    raise exception 'INVALID_CREDENTIALS' using errcode = '28P01';
  end if;
  select * into usr from users where id = v_token.user_id;

  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);

  update users set last_login = now() where id = usr.id;
  update otp_token set used_at = now() where id = v_token.id;
  return (usr, jwt);
end;
$$;

GRANT ALL ON FUNCTION otp_login TO anonymous;
select verify_function('public.otp_login');

drop function if exists register_using_invitation;

CREATE or replace FUNCTION public.register_using_invitation(email text, passwd text, token uuid, login text default null)
  RETURNS login_result
  LANGUAGE plpgsql SECURITY DEFINER
  AS $$
declare
  invitation person_invitation;
  v_salt text;
  usr users;
  jwt jwt_token;
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
  return (usr, jwt);
end
$$;

select verify_function('register_using_invitation');
GRANT ALL ON FUNCTION public.register_using_invitation TO anonymous;

drop function if exists register_without_invitation;

CREATE or replace FUNCTION register_without_invitation(email text, passwd text) RETURNS login_result
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  v_salt text;
  usr users;
  jwt jwt_token;
begin
  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;
  insert into users (u_email, u_pass) values (email, encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex')) returning * into usr;
  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);
  return (usr, jwt);
end
$$;

GRANT ALL ON FUNCTION register_without_invitation TO anonymous;

drop function if exists app_private.log_in_as;
drop function if exists log_in_as;

CREATE FUNCTION public.log_in_as(id bigint) RETURNS login_result
  LANGUAGE sql STRICT SECURITY DEFINER
  SET search_path TO pg_catalog, public, pg_temp
AS $$
  select (
    (select (users.*)::users from users where users.id = $1),
    (select app_private.create_jwt_token(users.*) from users where users.id = $1)
  );
$$;

GRANT ALL ON FUNCTION public.log_in_as TO administrator;

CREATE or replace FUNCTION login(login text, passwd text) RETURNS login_result
  LANGUAGE plpgsql SECURITY DEFINER
  SET search_path TO pg_catalog, public, pg_temp
AS $$
declare
  v_salt text;
  usr users;
  jwt jwt_token;
begin
  select encode(digest('######TK.-.OLYMP######', 'md5'), 'hex') into v_salt;

  select u.* into usr
  from users u
  where (lower(u.u_login) = lower(trim(login)) or lower(u.u_email) = lower(trim(login)))
    and u.u_pass = encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex');

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
  return (usr, jwt);
end;
$$;

select verify_function('login');
GRANT ALL ON FUNCTION login TO anonymous;

CREATE or replace FUNCTION set_lesson_demand(registration_id bigint, trainer_id bigint, lesson_count integer) RETURNS event_lesson_demand
  LANGUAGE plpgsql STRICT SECURITY DEFINER
  SET search_path TO pg_catalog, public, pg_temp
AS $$
declare
  v_event event;
  registration event_registration;
  lesson_demand event_lesson_demand;
begin
  select * into registration from event_registration where id = $1;
  select * into v_event from event where id = registration.event_id;

  if v_event is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  if v_event.is_locked = true then
    raise exception 'NOT_ALLOWED' using errcode = '28000';
  end if;

  if $3 = 0 then
    delete from event_lesson_demand eld where eld.registration_id = registration.id and eld.trainer_id = $2;
    return null;
  end if;

  INSERT INTO event_lesson_demand (registration_id, trainer_id, lesson_count)
  values ($1, $2, $3)
  on conflict on constraint eld_unique_registration_trainer_key do update set lesson_count = $3
  returning * into lesson_demand;

  return lesson_demand;
end;
$$;

select verify_function('set_lesson_demand');
GRANT ALL ON FUNCTION set_lesson_demand(registration_id bigint, trainer_id bigint, lesson_count integer) TO anonymous;
