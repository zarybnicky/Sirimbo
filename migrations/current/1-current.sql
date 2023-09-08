drop function if exists users_has_valid_payment;
drop function if exists person_has_user;
drop table if exists app_private.parameters;

alter table users
  drop column if exists u_poznamky,
  drop column if exists u_system,
  drop column if exists u_member_since,
  drop column if exists u_member_until,
  drop column if exists u_gdpr_signed_at,
  drop column if exists u_nationality;

select app_private.drop_policies('public.person');
drop materialized view if exists app_private.auth_details;
drop view if exists app_private.auth_details;
drop materialized view if exists auth_details;
create materialized view if not exists auth_details as
  SELECT
    person.id as person_id,
    array_remove(array_agg(couple.id), null) as couple_ids,
    array_remove(array_agg(cohort_id), null) as cohort_memberships,
    array_remove(array_agg(tenant_membership.tenant_id), null) tenant_memberships,
    array_remove(array_agg(tenant_trainer.tenant_id), null) tenant_trainers,
    array_remove(array_agg(tenant_administrator.tenant_id), null) tenant_administrators,
    array_remove(array_agg(tenant_administrator.tenant_id) || array_agg(tenant_trainer.tenant_id) || array_agg(tenant_membership.tenant_id), null) allowed_tenants --
  from person
  left join couple on (person.id=couple.man_id or person.id=couple.woman_id) and now() <@ couple.active_range
  left join cohort_membership on person.id=cohort_membership.person_id and now() <@ cohort_membership.active_range
  left join tenant_membership on person.id=tenant_membership.person_id and now() <@ tenant_membership.active_range
  left join tenant_trainer on person.id=tenant_trainer.person_id and now() <@ tenant_trainer.active_range
  left join tenant_administrator on person.id=tenant_administrator.person_id and now() <@ tenant_administrator.active_range
  group by person.id;
create unique index on auth_details (person_id);
grant all on auth_details to anonymous;
comment on materialized view auth_details is E'@omit';

create policy admin_all on person to administrator using (true);
create policy admin_myself on person for update using (id in (select my_person_ids()));
create policy view_tenant_or_trainer on person for select using (
  true = (
    select current_tenant_id() = any (allowed_tenants) and (
      current_tenant_id() in (select my_tenant_ids())
      or current_tenant_id() = any (tenant_trainers || tenant_administrators)
    )
    from auth_details where person_id=id
  )
);

create or replace function event_is_registration_open(e event) returns boolean language sql stable as $$
  select not e.is_locked;
$$;
grant all on function event_is_registration_open to anonymous;

select app_private.drop_policies('public.event_registration');
create policy admin_all on event_registration to administrator using (true);
create policy update_my on event_registration for update using (
  (select event_is_registration_open(event) from event where event_id = event.id)
  and (person_id in (select my_person_ids()) or couple_id in (select my_couple_ids()))
);
create policy delete_my on event_registration for delete using (
  (select event_is_registration_open(event) from event where event_id = event.id)
  and (person_id in (select my_person_ids()) or couple_id in (select my_couple_ids()))
);
create policy view_visible_event on event_registration for select using (
  exists (select 1 from event where event_id = event.id)
);

select app_private.drop_policies('public.event_trainer');
create policy admin_all on event_trainer to administrator using (true);
create policy view_tenant on event_trainer for select using (tenant_id = current_tenant_id());

select app_private.drop_policies('public.event_target_cohort');
create policy admin_all on event_target_cohort to administrator using (true);
create policy view_tenant on event_target_cohort for select using (tenant_id = current_tenant_id());

select app_private.drop_policies('public.event_instance_trainer');
create policy admin_all on event_instance_trainer to administrator using (true);
create policy view_tenant on event_instance_trainer for select using (tenant_id = current_tenant_id());

CREATE or replace FUNCTION my_person_ids() RETURNS setof bigint LANGUAGE sql STABLE rows 5 AS $$
  SELECT json_array_elements_text(nullif(current_setting('jwt.claims.my_person_ids', true), '')::json)::bigint;
$$;
CREATE or replace FUNCTION my_tenant_ids() RETURNS setof bigint LANGUAGE sql STABLE rows 5 AS $$
  SELECT json_array_elements_text(nullif(current_setting('jwt.claims.my_tenant_ids', true), '')::json)::bigint;
$$;
CREATE or replace FUNCTION my_cohort_ids() RETURNS setof bigint LANGUAGE sql STABLE rows 5 AS $$
  SELECT json_array_elements_text(nullif(current_setting('jwt.claims.my_cohort_ids', true), '')::json)::bigint;
$$;
CREATE or replace FUNCTION my_couple_ids() RETURNS setof bigint LANGUAGE sql STABLE rows 5 AS $$
  SELECT json_array_elements_text(nullif(current_setting('jwt.claims.my_couple_ids', true), '')::json)::bigint;
$$;


select app_private.drop_policies('public.cohort_membership');
CREATE POLICY admin_all ON public.cohort_membership TO administrator USING (true);
CREATE POLICY view_all ON public.cohort_membership FOR SELECT USING (current_tenant_id() = tenant_id);

select cron.schedule('refresh auth_details', '5 seconds', 'refresh materialized view concurrently auth_details;');
select cron.schedule('prune cron logs', '0 0 * * *', 'DELETE FROM cron.job_run_details WHERE end_time < now() - interval ''7 days''');

drop function if exists filtered_people;
CREATE or replace FUNCTION public.filtered_people(in_cohort bigint, is_trainer boolean, is_admin boolean) RETURNS SETOF person LANGUAGE sql STABLE AS $$
  select person.* from person
  join auth_details on person_id=person.id
  where
    current_tenant_id() = any (auth_details.allowed_tenants) and
    case when in_cohort is null then true else in_cohort = any (auth_details.cohort_memberships) end
    and case when is_trainer is null then true else is_trainer = (current_tenant_id() = any (auth_details.tenant_trainers)) end
    and case when is_admin is null then true else is_admin = (current_tenant_id() = any (auth_details.tenant_administrators)) end
  order by last_name, first_name
$$;
COMMENT ON FUNCTION public.filtered_people IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.filtered_people TO anonymous;

CREATE or replace FUNCTION app_private.create_jwt_token(u public.users) RETURNS public.jwt_token
    LANGUAGE sql STABLE
    AS $$
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
    array_agg(person_id) as my_person_ids,
    array_accum(my_tenant_ids) as my_tenant_ids,
    array_accum(my_cohort_ids) as my_cohort_ids,
    array_accum(my_couple_ids) as my_couple_ids,
    bool_or(is_member) as is_member,
    bool_or(is_trainer) as is_trainer,
    bool_or(is_admin) as is_admin
  from details group by user_id;
$$;

CREATE or replace FUNCTION person_is_admin(p person) RETURNS boolean LANGUAGE sql STABLE AS $$
  select current_tenant_id() = any (tenant_administrators) from auth_details where person_id=p.id;
$$;
GRANT ALL ON FUNCTION person_is_admin TO anonymous;

CREATE or replace FUNCTION person_is_trainer(p person) RETURNS boolean LANGUAGE sql STABLE AS $$
  select current_tenant_id() = any (tenant_trainers) from auth_details where person_id=p.id;
$$;
GRANT ALL ON FUNCTION person_is_admin TO anonymous;

CREATE or replace FUNCTION person_is_member(p person) RETURNS boolean LANGUAGE sql STABLE AS $$
  select current_tenant_id() = any (tenant_memberships) from auth_details where person_id=p.id;
$$;
GRANT ALL ON FUNCTION person_is_member TO anonymous;


create or replace function delete_event_instance(id bigint, out deleted event_instance) language plpgsql strict as $$
declare
  inst event_instance;
begin
  select * into inst from event_instance where event_instance.id = $1;
  if inst is null then
    raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
  end if;
  delete from event_instance where event_instance.id=inst.id returning * into deleted;
  if (select count(*) < 2 from event_instance where event_instance.event_id = inst.event_id) then
    delete from event where event.id=inst.event_id;
  end if;
end
$$;
select verify_function('delete_event_instance');

create or replace function invitation_info(token uuid) returns text language sql stable security definer as $$
  select email from person_invitation where access_token=token and used_at is null;
$$;
grant all on function invitation_info to anonymous;

create or replace function event_registrants(e event) returns setof person language sql stable as $$
  select person.* from person where id in (
    select unnest(array[person_id, man_id, woman_id]) as id
    from event_registration left join couple on couple.id = couple_id
    where event_id=e.id
  ) order by last_name asc, first_name asc;
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
  insert into users (u_login, u_email, u_pass) values (login, email, encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex')) returning * into usr;
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
