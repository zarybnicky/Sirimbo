--! Previous: sha1:52e81a7f345f8ef0bd36ae1ee5068c248508d3af
--! Hash: sha1:081f2239dfcd68c8a890b91ca2a07d0aae36db9d

--! split: 1-current.sql
revoke all on table tenant from anonymous;
grant select on table tenant to anonymous;
grant update (name, description, bank_account, cz_ico, cz_dic, address) on table tenant to administrator;
grant all on table tenant to system_admin;

drop trigger if exists _200_refresh_auth_details on cohort_membership;
drop trigger if exists _200_refresh_auth_details on couple;
drop trigger if exists _200_refresh_auth_details on tenant_membership;
drop trigger if exists _200_refresh_auth_details on tenant_trainer;
drop trigger if exists _200_refresh_auth_details on tenant_administrator;
drop function if exists app_private.tg_auth_details__refresh();

select app_private.drop_policies('public.tenant');
create policy system_admin_all on tenant to system_admin using (true);
create policy admin_all on tenant to administrator using (id = (select current_tenant_id()));
create policy public_view on tenant for select to anonymous using (id = (select current_tenant_id()));

comment on table tenant is '@omit create,delete
@behavior -singularRelation:resource:single -query:resource:connection
@simpleCollections only';

comment on table tenant_membership is '@simpleCollections both
@behavior -query:resource:list -query:resource:connection';
comment on table tenant_trainer is '@simpleCollections both
@behavior -query:resource:list -query:resource:connection';
comment on table tenant_administrator is '@simpleCollections both
@behavior -query:resource:list -query:resource:connection';
comment on table tenant_settings is '@omit create,delete
@behavior -query:resource:list -query:resource:connection -singularRelation:resource:list';

alter table access_event add column if not exists location_id bigint;

do $$
begin
  if not exists (
    select 1
    from pg_constraint
    where conname = 'access_event_location_fkey'
      and conrelid = 'access_event'::regclass
  ) then
    alter table access_event
      add constraint access_event_location_fkey
      foreign key (tenant_id, location_id)
      references tenant_location (tenant_id, id);
  end if;
end;
$$;

update access_event e
set location_id = 12
where e.location_id is null
  and e.device = 'ASI1201E-V1'
  and exists (
    select 1
    from tenant_location l
    where l.tenant_id = e.tenant_id
      and l.id = 12
  );

create index if not exists access_event_location_idx
  on access_event (tenant_id, location_id, occurred_at desc);

alter table access_credential
  add column if not exists status relationship_status not null default 'active';

update access_credential
set status = app_private.relationship_status_next(now(), valid_range, status)
where status is distinct from app_private.relationship_status_next(now(), valid_range, status);

create table if not exists security_event (
  id bigint generated always as identity primary key,
  tenant_id bigint not null default current_tenant_id() references tenant (id) on delete cascade,
  user_id bigint references users (id) on delete set null,
  person_id bigint references person (id) on delete set null,
  actor_user_id bigint default current_user_id() references users (id) on delete set null,
  kind text not null,
  method text not null check (method in ('password', 'otp', 'manual', 'scheduled')),
  occurred_at timestamptz not null default now(),
  effective_at timestamptz not null default now()
);

create index if not exists security_event_tenant_occurred_at_idx
  on security_event (tenant_id, occurred_at desc);
create index if not exists security_event_user_occurred_at_idx
  on security_event (user_id, occurred_at desc);
create index if not exists security_event_person_occurred_at_idx
  on security_event (person_id, occurred_at desc);
create index if not exists security_event_actor_user_id_idx
  on security_event (actor_user_id);

comment on table security_event is '@omit create,update,delete
@simpleCollections only
@behavior -query:resource:list -query:resource:connection -query:resource:single';
comment on constraint security_event_tenant_id_fkey on security_event is
  '@behavior -manyRelation:resource:list -manyRelation:resource:connection';
comment on constraint security_event_actor_user_id_fkey on security_event is
  '@behavior -manyRelation:resource:list -manyRelation:resource:connection';

alter table security_event enable row level security;
grant select on table security_event to member;

select app_private.drop_policies('public.security_event');
create policy current_tenant on security_event as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy self_view on security_event for select to member
  using (
    user_id = (select current_user_id())
    or person_id = any ((select current_person_ids())::bigint[])
  );
create policy admin_view on security_event for select to administrator using (true);

--! Included functions/cron_update_memberships.sql
create or replace function app_private.relationship_status_next(ts timestamptz, range tstzrange, current relationship_status)
  returns relationship_status
  language sql
  immutable
as $$
  select case
    when ts < lower(range) then 'pending'
    when not upper_inf(range) and ts >= upper(range) then 'expired'
    when range @> ts then 'active'
    else current
  end
$$;

create or replace function app_private.cron_update_memberships() returns void language sql
as $$
  UPDATE user_proxy SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE couple SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE cohort_membership SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE tenant_membership SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE tenant_trainer SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE tenant_administrator SET status = app_private.relationship_status_next(now(), active_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), active_range, status);

  UPDATE access_credential SET status = app_private.relationship_status_next(now(), valid_range, status)
  WHERE status IS DISTINCT FROM app_private.relationship_status_next(now(), valid_range, status);

  UPDATE announcement
  SET status = app_private.announcement_status_next(now(), scheduled_since, scheduled_until, status)
  WHERE status IN ('scheduled', 'published')
    AND status IS DISTINCT FROM app_private.announcement_status_next(now(), scheduled_since, scheduled_until, status);
$$;
--! EndIncluded functions/cron_update_memberships.sql
--! Included functions/security_events.sql
create or replace function app_private.tg_relationship__status()
  returns trigger
  language plpgsql
as $$
begin
  new.status := app_private.relationship_status_next(
    now(), tstzrange(new.since, new.until, '[)'), new.status
  );
  return new;
end;
$$;

create or replace function app_private.tg_security_event__range()
  returns trigger
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
begin
  if tg_op = 'DELETE' then
    if old.status = 'active' then
      insert into security_event (person_id, kind, method)
      values (old.person_id, tg_argv[1], 'manual');
    end if;
    return old;
  end if;

  if tg_op = 'INSERT' and new.status = 'active' then
    insert into security_event (person_id, kind, method, effective_at)
    values (new.person_id, tg_argv[0], 'manual', new.since);
  elsif tg_op = 'UPDATE'
     and new.status is distinct from old.status
     and new.status in ('active', 'expired') then
    insert into security_event (person_id, kind, method, effective_at)
    values (
      new.person_id,
      case when new.status = 'active' then tg_argv[0] else tg_argv[1] end,
      case when current_user_id() is null then 'scheduled' else 'manual' end,
      case when new.status = 'active' then new.since else new.until end
    );
  end if;
  return new;
end;
$$;

create or replace function app_private.tg_security_event__credential()
  returns trigger
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
begin
  if tg_op = 'DELETE' then
    if old.status = 'active' then
      insert into security_event (person_id, kind, method)
      values (old.person_id, 'access_credential_ended', 'manual');
    end if;
    return old;
  end if;

  if tg_op = 'INSERT' and new.status = 'active' then
    insert into security_event (person_id, kind, method, effective_at)
    values (new.person_id, 'access_credential_issued', 'manual', new.since);
  elsif tg_op = 'UPDATE'
     and new.status is distinct from old.status
     and new.status in ('active', 'expired') then
    insert into security_event (person_id, kind, method, effective_at)
    values (
      new.person_id,
      case when new.status = 'active' then 'access_credential_issued' else 'access_credential_ended' end,
      case when current_user_id() is null then 'scheduled' else 'manual' end,
      case when new.status = 'active' then new.since else new.until end
    );
  end if;
  return new;
end;
$$;

create or replace function app_private.tg_security_event__user_proxy()
  returns trigger
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
begin
  if tg_op = 'DELETE' then
    if old.status = 'active' then
      insert into security_event (user_id, person_id, kind, method)
      values (old.user_id, old.person_id, 'person_unlinked', 'manual');
    end if;
    return old;
  end if;

  if tg_op = 'INSERT' and new.status = 'active' then
    insert into security_event (user_id, person_id, kind, method, effective_at)
    values (new.user_id, new.person_id, 'person_linked', 'manual', new.since);
  elsif tg_op = 'UPDATE'
     and new.status is distinct from old.status
     and new.status in ('active', 'expired') then
    insert into security_event (user_id, person_id, kind, method, effective_at)
    values (
      new.user_id,
      new.person_id,
      case when new.status = 'active' then 'person_linked' else 'person_unlinked' end,
      case when current_user_id() is null then 'scheduled' else 'manual' end,
      case when new.status = 'active' then new.since else new.until end
    );
  end if;
  return new;
end;
$$;

create or replace function app_private.tg_security_event__password()
  returns trigger
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
begin
  insert into security_event (user_id, kind, method)
  values (new.id, 'password_changed', 'manual');
  return new;
end;
$$;

create or replace trigger _050_relationship_status
  before insert or update of since, until on user_proxy
  for each row execute function app_private.tg_relationship__status();
create or replace trigger _050_relationship_status
  before insert or update of since, until on tenant_membership
  for each row execute function app_private.tg_relationship__status();
create or replace trigger _050_relationship_status
  before insert or update of since, until on tenant_trainer
  for each row execute function app_private.tg_relationship__status();
create or replace trigger _050_relationship_status
  before insert or update of since, until on tenant_administrator
  for each row execute function app_private.tg_relationship__status();
create or replace trigger _050_relationship_status
  before insert or update of since, until on access_credential
  for each row execute function app_private.tg_relationship__status();

create or replace trigger _900_security_event
  after insert or update of status or delete on tenant_membership
  for each row execute function app_private.tg_security_event__range(
    'membership_granted', 'membership_revoked'
  );
create or replace trigger _900_security_event
  after insert or update of status or delete on tenant_trainer
  for each row execute function app_private.tg_security_event__range(
    'trainer_granted', 'trainer_revoked'
  );
create or replace trigger _900_security_event
  after insert or update of status or delete on tenant_administrator
  for each row execute function app_private.tg_security_event__range(
    'administrator_granted', 'administrator_revoked'
  );
create or replace trigger _900_security_event
  after insert or update of status or delete on access_credential
  for each row execute function app_private.tg_security_event__credential();
create or replace trigger _900_security_event
  after insert or update of status or delete on user_proxy
  for each row execute function app_private.tg_security_event__user_proxy();
create or replace trigger _900_security_event
  after update of u_pass on users
  for each row
  when (old.u_pass is distinct from new.u_pass)
  execute function app_private.tg_security_event__password();
--! EndIncluded functions/security_events.sql
--! Included functions/login.sql
CREATE or replace FUNCTION login(login text, passwd text) RETURNS login_result
  LANGUAGE plpgsql SECURITY DEFINER
  SET search_path TO pg_catalog, public, pg_temp
AS $$
declare
  v_salt text;
  usr users;
  jwt jwt_token;
begin
  v_salt := encode(digest('######TK.-.OLYMP######', 'md5'), 'hex');

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
  insert into security_event (user_id, kind, method)
  values (usr.id, 'login_succeeded', 'password');
  return (usr, jwt);
end;
$$;

GRANT ALL ON FUNCTION login TO anonymous;
--! EndIncluded functions/login.sql
--! Included functions/otp_login.sql
drop function if exists otp_login;

CREATE or replace FUNCTION otp_login(token uuid)
  RETURNS login_result
  LANGUAGE plpgsql
  STRICT SECURITY DEFINER
  SET search_path TO pg_catalog, public, pg_temp
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
  insert into security_event (user_id, kind, method)
  values (usr.id, 'login_succeeded', 'otp');
  return (usr, jwt);
end;
$$;

GRANT ALL ON FUNCTION otp_login TO anonymous;
--! EndIncluded functions/otp_login.sql
--! Included functions/reset_password.sql
CREATE or replace FUNCTION reset_password(email character varying) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    SET search_path TO pg_catalog, public, pg_temp
    AS $$
declare
  v_tenant tenant;
  v_user users;
  v_token otp_token;
  v_payload jsonb := jsonb_build_array();
begin
  for v_user in (select * from users where u_email = email) loop
    insert into otp_token (user_id)
    values (v_user.id) returning * into v_token;

    insert into security_event (user_id, kind, method)
    values (v_user.id, 'password_reset_requested', 'manual');

    v_payload := v_payload || jsonb_build_object(
      'login', v_user.u_login,
      'email', v_user.u_email,
      'token', v_token.access_token,
      'people', (
        select jsonb_agg(person.name)
        from user_proxy join person on person_id=person.id
        where status = 'active' and user_id = v_user.id
      )
    );
  end loop;

  select * into v_tenant from tenant where id = current_tenant_id();

  if v_payload is not null then
    perform graphile_worker.add_job('forgotten_password_generate', json_build_object(
      'origin', v_tenant.origins[1],
      'intent', '/zapomenute-heslo',
      'users', v_payload
    ));
  end if;
end;
$$;

GRANT ALL ON FUNCTION reset_password TO anonymous;
--! EndIncluded functions/reset_password.sql
--! Included functions/change_password.sql
CREATE or replace FUNCTION change_password(new_pass text) RETURNS void
    LANGUAGE plpgsql STRICT
    AS $$
begin
  update users set u_pass = new_pass
  where id = current_user_id();
end;
$$;

GRANT ALL ON FUNCTION change_password(new_pass text) TO anonymous;
--! EndIncluded functions/change_password.sql
--! Included functions/register_without_invitation.sql
CREATE or replace FUNCTION register_without_invitation(email text, passwd text) RETURNS login_result
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    SET search_path TO pg_catalog, public, pg_temp
    AS $$
declare
  v_salt text;
  usr users;
  jwt jwt_token;
begin
  v_salt := encode(digest('######TK.-.OLYMP######', 'md5'), 'hex');
  insert into users (u_email, u_pass) values (email, encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex')) returning * into usr;
  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);
  insert into security_event (user_id, kind, method)
  values (usr.id, 'registration', 'manual');
  return (usr, jwt);
end
$$;

GRANT ALL ON FUNCTION register_without_invitation TO anonymous;
--! EndIncluded functions/register_without_invitation.sql
--! Included functions/register_using_invitation.sql
create or replace function register_using_invitation(email text, passwd text, token uuid, login text default null)
  returns login_result
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
  as $$
declare
  invitation person_invitation;
  v_user_id bigint := current_user_id();
  v_salt text;
  usr users;
  jwt jwt_token;
  v_registered boolean := false;
begin
  select * into invitation from person_invitation where access_token=token for update;

  if invitation is null then
    raise exception 'INVITATION_NOT_FOUND' using errcode = '28000';
  end if;
  if invitation.used_at is not null then
    raise exception 'INVITATION_ALREADY_USED' using errcode = '28P01';
  end if;

  if v_user_id is null then
    if email is null or email = '' then
      raise exception 'INVALID_EMAIL' using errcode = '28P01';
    end if;

    v_salt := encode(digest('######TK.-.OLYMP######', 'md5'), 'hex');
    insert into users (u_login, u_email, u_pass) values (trim(login), email, encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex')) returning * into usr;
    v_registered := true;
  else
    select * into usr from users where id=v_user_id;
    if usr is null then
      raise exception 'INVALID_CREDENTIALS' using errcode = '28P01';
    end if;
  end if;

  perform set_config('jwt.claims.user_id', usr.id::text, true);
  insert into user_proxy (user_id, person_id) values (usr.id, invitation.person_id) on conflict do nothing;
  update person_invitation set used_at=now() where access_token=token;
  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);
  if v_registered then
    insert into security_event (user_id, kind, method)
    values (usr.id, 'registration', 'manual');
  end if;
  insert into security_event (user_id, person_id, kind, method)
  values (usr.id, invitation.person_id, 'invitation_accepted', 'manual');
  return (usr, jwt);
end
$$;

grant all on function register_using_invitation to anonymous;
--! EndIncluded functions/register_using_invitation.sql
--! Included functions/log_in_as.sql
drop function if exists log_in_as;

CREATE FUNCTION log_in_as(id bigint) RETURNS login_result
  LANGUAGE plpgsql
  STRICT SECURITY DEFINER
  SET search_path TO pg_catalog, public, pg_temp
AS $$
declare
  v_id alias for $1;
  usr users;
  jwt jwt_token;
begin
  select * into strict usr from users where users.id = v_id;
  jwt := app_private.create_jwt_token(usr);
  insert into security_event (user_id, kind, method)
  values (usr.id, 'impersonation', 'manual');
  return (usr, jwt);
end;
$$;

revoke all on FUNCTION log_in_as from anonymous;
GRANT ALL ON FUNCTION log_in_as TO administrator;
--! EndIncluded functions/log_in_as.sql
--! Included functions/confirm_membership_application.sql
drop function if exists confirm_membership_application(bigint);

create or replace function confirm_membership_application(
  application_id bigint,
  is_member boolean default true,
  is_trainer boolean default false,
  is_admin boolean default false,
  join_date timestamptz default now(),
  cohort_ids bigint[] default array[]::bigint[]
)
  returns person
  language sql
as $$
  with application as materialized (
    select *
    from membership_application
    where id = application_id and status = 'sent'
    for update
  ), t_person as (
    insert into person (
      first_name, last_name, gender, birth_date, nationality, tax_identification_number,
      national_id_number, csts_id, wdsf_id, prefix_title, suffix_title, bio, email, phone,
      note
    )
    select
      first_name, last_name, gender, birth_date, nationality, tax_identification_number,
      national_id_number, csts_id, wdsf_id, prefix_title, suffix_title, bio, email, phone,
      note
    from application
    returning *
  ), appl as (
    update membership_application
    set status = 'approved'
    where id = (select id from application)
  ), member as (
    insert into tenant_membership (tenant_id, person_id, since)
    select current_tenant_id(), id, join_date from t_person where is_member
  ), trainer as (
    insert into tenant_trainer (tenant_id, person_id, since)
    select current_tenant_id(), id, join_date from t_person where is_trainer
  ), administrator as (
    insert into tenant_administrator (tenant_id, person_id, since)
    select current_tenant_id(), id, join_date from t_person where is_admin
  ), cohorts as (
    insert into cohort_membership (cohort_id, person_id, since)
    select cohort_id, t_person.id, join_date
    from t_person
    cross join unnest(coalesce(cohort_ids, array[]::bigint[])) selected(cohort_id)
  ), proxy as (
    insert into user_proxy (person_id, user_id)
    select t_person.id, application.created_by
    from t_person cross join application
  )
  select * from t_person;
$$;

grant all on function confirm_membership_application to administrator;
--! EndIncluded functions/confirm_membership_application.sql

select app_private.drop_policies('public.tenant_settings');
create policy system_admin_all on tenant_settings to system_admin using (true);
create policy admin_own on tenant_settings to administrator using (tenant_id = (select current_tenant_id()));

select app_private.drop_policies('public.tenant_administrator');
create policy public_view on tenant_administrator for select using (true);
create policy system_admin_all on tenant_administrator to system_admin using (true);
create policy admin_all on tenant_administrator to administrator using (tenant_id = (select current_tenant_id()));

select app_private.drop_policies('public.tenant_trainer');
create policy public_view on tenant_trainer for select using (true);
create policy system_admin_all on tenant_trainer to system_admin using (true);
create policy admin_all on tenant_trainer to administrator using (tenant_id = (select current_tenant_id()));

select app_private.drop_policies('public.tenant_membership');
create policy view_visible_person on tenant_membership for select using (true);
create policy system_admin_all on tenant_membership to system_admin using (true);
create policy admin_all on tenant_membership to administrator using (tenant_id = (select current_tenant_id()));
