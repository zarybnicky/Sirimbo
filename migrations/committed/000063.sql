--! Previous: sha1:c13221efa223dfd1fa19945eb9b77962e24405f6
--! Hash: sha1:2be3fd8e627decbc6f3b8e3698c76026e55b0f35

--! split: 1-current.sql
create or replace function get_current_user(version_id text default null) returns users
language sql volatile security definer as $$
  with updated_user as (
    update users
    set
      last_active_at = now(),
      last_version = version_id
    where id = nullif(current_setting('jwt.claims.user_id', true), '')::integer
    returning *
  )
  select * from updated_user
  union all
  select *
  from users
  where id = nullif(current_setting('jwt.claims.user_id', true), '')::integer
    and not exists (select 1 from updated_user);
$$;

grant all on function get_current_user to anonymous;

CREATE or replace FUNCTION app_private.create_jwt_token(u public.users) RETURNS public.jwt_token
    LANGUAGE sql STABLE
    AS $$
  with details as (
    SELECT
      users.id,
      user_proxy.person_id as person_id,
      tenant_memberships || tenant_trainers || tenant_administrators as my_tenant_ids,
      cohort_memberships as my_cohort_ids,
      couple_ids as my_couple_ids,
      current_tenant_id() = ANY (tenant_memberships || tenant_trainers || tenant_administrators) as is_member,
      current_tenant_id() = ANY (tenant_trainers) as is_trainer,
      current_tenant_id() = ANY (tenant_administrators) as is_admin
    from users
    left join user_proxy on user_id=users.id
    left join auth_details on user_proxy.person_id=auth_details.person_id
    where users.id=u.id
  ) select
    extract(epoch from now() + interval '7 days')::integer,
    u.id,
    current_tenant_id(),
    u.u_login,
    u.u_email,
    array_to_json(array_agg(person_id)) as my_person_ids,
    array_to_json(app_private.array_accum(my_tenant_ids)) as my_tenant_ids,
    array_to_json(app_private.array_accum(my_cohort_ids)) as my_cohort_ids,
    array_to_json(app_private.array_accum(my_couple_ids)) as my_couple_ids,
    bool_or(is_member) as is_member,
    bool_or(is_trainer) as is_trainer,
    bool_or(is_admin) as is_admin
  from details group by id;
$$;

CREATE or replace FUNCTION refresh_jwt() RETURNS jwt_token
    LANGUAGE sql STABLE SECURITY DEFINER
    AS $$
  SELECT app_private.create_jwt_token(users) FROM users WHERE id = nullif(current_setting('jwt.claims.user_id', true), '')::integer;
$$;

GRANT ALL ON FUNCTION public.refresh_jwt() TO anonymous;

CREATE or replace FUNCTION public.change_password(new_pass text) RETURNS void
    LANGUAGE sql STRICT
    AS $$
  update users set u_pass = new_pass where id = current_user_id();
$$;

GRANT ALL ON FUNCTION public.change_password(new_pass text) TO anonymous;

CREATE or replace FUNCTION public.reset_password(email character varying) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  v_tenant tenant;
  v_user users;
  v_token otp_token;
  v_people jsonb;
  v_payload jsonb := null;
begin
  for v_user in (select * from users where u_email = email) loop
    insert into otp_token (user_id)
    values (v_user.id) returning * into v_token;

    select jsonb_agg(person_name(person.*)) into v_people
    from user_proxy join person on person_id=person.id
    where active and user_id = v_user.id;

    v_payload := coalesce(v_payload, jsonb_build_array()) || jsonb_build_object(
      'login', v_user.u_login,
      'email', v_user.u_email,
      'token', v_token.access_token,
      'people', v_people
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

GRANT ALL ON FUNCTION public.reset_password(email character varying) TO anonymous;
select verify_function('reset_password');

CREATE or replace FUNCTION otp_login(token uuid, OUT usr users, OUT jwt jwt_token) RETURNS record
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  v_token otp_token;
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
end;
$$;

GRANT ALL ON FUNCTION otp_login TO anonymous;
--select verify_function('public.otp_login');


create or replace function app_private.queue_announcement_notifications(in_announcement_id bigint)
returns void
language plpgsql
security definer
as $$
declare
  v_tenant_id bigint;
  v_is_visible boolean;
  v_since timestamptz;
  v_until timestamptz;
  v_user_ids bigint[];
begin
  select tenant_id,
         coalesce(is_visible, false),
         scheduled_since,
         scheduled_until
  into v_tenant_id,
    v_is_visible,
    v_since,
    v_until
  from announcement
  where id = in_announcement_id;

  if not found then
    return;
  end if;

  if not (v_is_visible
      and (v_since is null or v_since <= now())
      and (v_until is null or v_until > now())) then
    return;
  end if;

  with role_flags as (
    select
      coalesce(bool_or(audience_role = 'member'), false) as has_member,
      coalesce(bool_or(audience_role = 'trainer'), false) as has_trainer,
      coalesce(bool_or(audience_role = 'administrator'), false) as has_administrator
    from announcement_audience
    where announcement_id = in_announcement_id
      and audience_role is not null
  ),
  role_people as (
    select distinct ad.person_id
    from auth_details ad
    join role_flags rf on rf.has_member or rf.has_trainer or rf.has_administrator
    where (
      rf.has_member and v_tenant_id = any (coalesce(ad.tenant_memberships, '{}'::bigint[]))
    ) or (
      rf.has_trainer and v_tenant_id = any (coalesce(ad.tenant_trainers, '{}'::bigint[]))
    ) or (
      rf.has_administrator and v_tenant_id = any (coalesce(ad.tenant_administrators, '{}'::bigint[]))
    )
  ),
  role_users as (
    select distinct u.id as user_id
    from role_people rp
    join user_proxy up on up.person_id = rp.person_id and up.active
    join users u on u.id = up.user_id
    where u.tenant_id = v_tenant_id
  ),
  cohort_users as (
    select distinct u.id as user_id
    from announcement_audience aa
    join cohort_membership cm
      on cm.cohort_id = aa.cohort_id
     and cm.active
    join user_proxy up on up.person_id = cm.person_id and up.active
    join users u on u.id = up.user_id
    where aa.announcement_id = in_announcement_id
      and aa.cohort_id is not null
      and aa.tenant_id = v_tenant_id
      and cm.tenant_id = v_tenant_id
      and u.tenant_id = v_tenant_id
  )
  select array_agg(distinct user_id order by user_id)
  into v_user_ids
  from (
    select user_id from role_users
    union
    select user_id from cohort_users
  ) recipients;

  if v_user_ids is null or array_length(v_user_ids, 1) = 0 then
    return;
  end if;

  perform graphile_worker.add_job(
    'notify_announcement',
    json_build_object(
      'announcement_id', in_announcement_id,
      'user_ids', v_user_ids
    )
  );
end;
$$;
select verify_function('app_private.queue_announcement_notifications');
comment on function app_private.queue_announcement_notifications is '@omit';
grant all on function app_private.queue_announcement_notifications to anonymous;

create or replace function app_private.tg_announcement__after_write()
returns trigger
language plpgsql
security definer
as $$
-- @plpgsql_check_options: oldtable = oldtable, newtable = newtable
declare
  rec record;
  old_row record;
  was_published boolean;
  is_published boolean;
begin
  for rec in
    select * from newtable
  loop
    is_published := coalesce(rec.is_visible, false)
      and (rec.scheduled_since is null or rec.scheduled_since <= now())
      and (rec.scheduled_until is null or rec.scheduled_until > now());

    if TG_OP = 'INSERT' then
      was_published := false;
    else
      select * into old_row
      from oldtable
      where id = rec.id;

      if not found then
        was_published := false;
      else
        was_published := coalesce(old_row.is_visible, false)
          and (old_row.scheduled_since is null or old_row.scheduled_since <= now())
          and (old_row.scheduled_until is null or old_row.scheduled_until > now());
      end if;
    end if;

    if is_published and not was_published then
      perform app_private.queue_announcement_notifications(rec.id);
    end if;
  end loop;

  return null;
end;
$$;
select verify_function('app_private.tg_announcement__after_write', 'announcement');

create or replace function app_private.tg_announcement_audience__after_write()
returns trigger
language plpgsql
security definer
as $$
-- @plpgsql_check_options: oldtable = oldtable, newtable = newtable
declare
  rec record;
begin
  if TG_OP = 'DELETE' then
    for rec in (
      select distinct announcement_id from oldtable
    ) loop
      perform app_private.queue_announcement_notifications(rec.announcement_id);
    end loop;
  else
    for rec in (
      select distinct announcement_id from newtable
    ) loop
      perform app_private.queue_announcement_notifications(rec.announcement_id);
    end loop;
  end if;
  return null;
end;
$$;
select verify_function('app_private.tg_announcement_audience__after_write', 'announcement_audience');

drop trigger if exists _600_notify_announcement_insert on announcement;
drop trigger if exists _600_notify_announcement_update on announcement;
create trigger _600_notify_announcement_insert
  after insert on announcement
  referencing new table as newtable
  for each statement execute function app_private.tg_announcement__after_write();
create trigger _600_notify_announcement_update
  after update on announcement
  referencing new table as newtable old table as oldtable
  for each statement execute function app_private.tg_announcement__after_write();

drop trigger if exists _600_notify_announcement_audience_insert on announcement_audience;
drop trigger if exists _600_notify_announcement_audience_update on announcement_audience;
create trigger _600_notify_announcement_audience_insert
  after insert on announcement_audience
  referencing new table as newtable
  for each statement execute function app_private.tg_announcement_audience__after_write();
create trigger _600_notify_announcement_audience_update
  after update on announcement_audience
  referencing new table as newtable old table as oldtable
  for each statement execute function app_private.tg_announcement_audience__after_write();
