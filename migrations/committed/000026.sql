--! Previous: sha1:8ee5b16dcfdf103f8a21f380a6e3da583cb46f45
--! Hash: sha1:7990d6da739d3b439422e0236dba9504989b23f6

--! split: 1-current.sql
drop policy if exists my_tenant on skupiny;

comment on table person is '@omit create';

create or replace function skupiny_in_current_tenant(s skupiny) returns boolean language sql stable as $$
  select s.tenant_id = current_tenant_id();
$$;
grant all on function skupiny_in_current_tenant to anonymous;
comment on function skupiny_in_current_tenant is '@filterable';

CREATE or replace FUNCTION person_is_admin(p person) RETURNS boolean LANGUAGE sql STABLE security definer AS $$
  select current_tenant_id() = any (auth_details.tenant_administrators) from app_private.auth_details where person_id=p.id;
$$;
GRANT ALL ON FUNCTION person_is_admin TO anonymous;

CREATE or replace FUNCTION person_is_trainer(p person) RETURNS boolean LANGUAGE sql STABLE security definer AS $$
  select current_tenant_id() = any (auth_details.tenant_trainers) from app_private.auth_details where person_id=p.id;
$$;
GRANT ALL ON FUNCTION person_is_admin TO anonymous;

CREATE or replace FUNCTION person_is_member(p person) RETURNS boolean LANGUAGE sql STABLE security definer AS $$
  select current_tenant_id() = any (auth_details.tenant_memberships) from app_private.auth_details where person_id=p.id;
$$;
GRANT ALL ON FUNCTION person_is_member TO anonymous;

comment on table user_proxy is E'@simpleCollections only';
comment on table couple is E'@simpleCollections only';
comment on table cohort_membership is E'@simpleCollections only';
comment on table tenant_membership is E'@simpleCollections only';
comment on table tenant_administrator is E'@simpleCollections only';
comment on table tenant_trainer is E'@simpleCollections only';

comment on table event_registration is E'@omit update
@simpleCollections both';

drop function if exists person_tenant_ids;

CREATE or replace FUNCTION public.event_instances_for_range(only_mine boolean, only_type event_type, start_range timestamptz, end_range timestamptz default null) RETURNS SETOF public.event_instance
    LANGUAGE sql STABLE
    AS $$
  select * from (
    select distinct on (event_instance.id) event_instance.*
    from event_instance
    join event on event_id=event.id
    left join event_registration on event_registration.event_id=event.id
    left join event_trainer on event_trainer.event_id=event.id
    left join event_instance_trainer on event_instance_trainer.instance_id=event_instance.id
    where only_mine
    and event.is_visible = true
    and tstzrange(start_range, end_range, '[]') && range
    and (only_type is null or event.type = only_type)
    and (
      event_registration.person_id in (select my_person_ids())
      or event_registration.couple_id in (select my_couple_ids())
      or event_trainer.person_id in (select my_person_ids())
      or event_instance_trainer.person_id in (select my_person_ids())
    )
    union
    select distinct on (event_instance.id) event_instance.*
    from event_instance
    join event on event_id=event.id
    where not only_mine
    and event.is_visible = true
    and tstzrange(start_range, end_range, '[]') && range
    and (only_type is null or event.type = only_type)
  ) a order by a.range;
$$;
COMMENT ON FUNCTION public.event_instances_for_range IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.event_instances_for_range TO anonymous;

-- explain analyze
--   select event_instance.*
--   from event_instance
--   join event on event_id=event.id
--   where event.is_visible = true
--   and tstzrange('2023-09-01', '2023-09-14', '[]') && range
--   and case true
--     when false then true
--     else exists (select 1 from event_registration where event_id=event.id and (person_id in (select my_person_ids()) or couple_id in (select my_couple_ids())))
--     or exists (select 1 from event_trainer where event_id=event.id and person_id in (select my_person_ids()))
--     or exists (select 1 from event_instance_trainer where instance_id=event_instance.id and person_id in (select my_person_ids()))
--   end
--   order by range asc;

-- savepoint testing;
-- select set_config('role', 'member', true), set_config('jwt.claims.user_id', '14', true), set_config('jwt.claims.tenant_id', '1', true), set_config('jwt.claims.username', 'superadmin', true), set_config('jwt.claims.email', 'tkolymp@tkolymp.cz', true), set_config('jwt.claims.my_person_ids', '[31]', true), set_config('jwt.claims.my_tenant_ids', '[1,2]', true), set_config('jwt.claims.my_cohort_ids', '[9]', true), set_config('jwt.claims.my_couple_ids', '[1]', true), set_config('jwt.claims.is_member', 'true', true), set_config('jwt.claims.is_trainer', 'false', true), set_config('jwt.claims.is_admin', 'false', true);

-- explain analyze
--   select * from (
--     select distinct on (event_instance.id) event_instance.*
--     from event_instance
--     join event on event_id=event.id
--     left join event_registration on event_registration.event_id=event.id
--     left join event_trainer on event_trainer.event_id=event.id
--     left join event_instance_trainer on event_instance_trainer.instance_id=event_instance.id
--     where event.is_visible = true
--     and tstzrange('2023-09-01', '2023-09-14', '[]') && range
--     and (
--       event_registration.person_id in (select my_person_ids())
--       or event_registration.couple_id in (select my_couple_ids())
--       or event_trainer.person_id in (select my_person_ids())
--       or event_instance_trainer.person_id in (select my_person_ids())
--     )
--   ) a order by a.range;

-- rollback to savepoint testing;


create or replace function event_my_registrations(e event) returns setof event_registration language sql stable as $$
  select * from event_registration
  where event_id=e.id
  and (person_id in (select my_person_ids()) or couple_id in (select my_couple_ids()));
$$;
COMMENT ON FUNCTION public.event_my_registrations IS '@simpleCollections only';
grant all on function event_my_registrations to anonymous;

-- insert into person_invitation (person_id, tenant_id, email)
-- select person.id, 2, person.email
-- from app_private.auth_details
--   join person on person.id=auth_details.person_id
--   left join user_proxy on auth_details.person_id=user_proxy.person_id
--   left join person_invitation on person_invitation.person_id=person.id
-- where (2 = any(tenant_memberships) or 2=any(tenant_trainers) or 2=any(tenant_administrators))
--   and (person.email is not null and person.email <> '')
--   and user_id is null and person_invitation.id is null;

comment on table person_invitation is E'@omit update
@simpleCollections only';
grant all on table person_invitation to anonymous;
alter table person_invitation enable row level security;
select app_private.drop_policies('public.person_invitation');
create policy admin_all on person_invitation to administrator using (true);
create policy admin_mine on person_invitation using (person_id in (select my_person_ids()));

drop function if exists login;
CREATE or replace FUNCTION public.login(login character varying, passwd character varying, OUT usr public.users, OUT jwt public.jwt_token) RETURNS record
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
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
  update users set last_login = now() where id = usr.id;
end;
$$;
GRANT ALL ON FUNCTION public.login TO anonymous;

drop function if exists register_using_invitation;
CREATE FUNCTION public.register_using_invitation(email text, login text, passwd text, token uuid, OUT usr public.users, OUT jwt public.jwt_token) RETURNS record
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
end
$$;
GRANT ALL ON FUNCTION public.register_using_invitation TO anonymous;

drop table if exists session;

CREATE or replace FUNCTION current_tenant_id() RETURNS bigint LANGUAGE sql STABLE AS $$
  select COALESCE(nullif(current_setting('jwt.claims.tenant_id', true), '')::bigint, 1);
$$;

drop view if exists app_private.auth_details;
create or replace view app_private.auth_details as
  SELECT
    person.id as person_id,
    array_agg(couple.id) as couple_ids,
    array_agg(cohort_id) as cohort_memberships,
    array_agg(tenant_membership.tenant_id) tenant_memberships,
    array_agg(tenant_trainer.tenant_id) tenant_trainers,
    array_agg(tenant_administrator.tenant_id) tenant_administrators
  from person
  left join couple on (person.id=couple.man_id or person.id=couple.woman_id) and now() <@ couple.active_range
  left join cohort_membership on person.id=cohort_membership.person_id and now() <@ cohort_membership.active_range
  left join tenant_membership on person.id=tenant_membership.person_id and now() <@ tenant_membership.active_range
  left join tenant_trainer on person.id=tenant_trainer.person_id and now() <@ tenant_trainer.active_range
  left join tenant_administrator on person.id=tenant_administrator.person_id and now() <@ tenant_administrator.active_range
  group by person.id;

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
    join app_private.auth_details on user_proxy.person_id=auth_details.person_id
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

--! split: 2-rls.sql
ALTER TABLE public.person_invitation
  DROP CONSTRAINT person_invitation_person_id_fkey;
ALTER TABLE public.person_invitation
  ADD CONSTRAINT person_invitation_person_id_fkey
  FOREIGN KEY (person_id)
  REFERENCES public.person (id)
  ON DELETE cascade;

ALTER TABLE public.person_invitation
  DROP CONSTRAINT person_invitation_tenant_id_fkey;
ALTER TABLE public.person_invitation
  ADD CONSTRAINT person_invitation_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE cascade;

ALTER TABLE app_private.crm_activity
  DROP CONSTRAINT crm_activity_prospect_fkey;
ALTER TABLE app_private.crm_activity
  ADD CONSTRAINT crm_activity_prospect_fkey
  FOREIGN KEY (prospect)
  REFERENCES app_private.crm_prospect (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.aktuality
  DROP CONSTRAINT aktuality_tenant_id_fkey;
ALTER TABLE public.aktuality
  ADD CONSTRAINT aktuality_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.attachment
  DROP CONSTRAINT attachment_uploaded_by_fkey;
ALTER TABLE public.attachment
  ADD CONSTRAINT attachment_uploaded_by_fkey
  FOREIGN KEY (uploaded_by)
  REFERENCES public.users (u_id)
  ON DELETE set null;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.cohort_group
  DROP CONSTRAINT cohort_group_tenant_id_fkey;
ALTER TABLE public.cohort_group
  ADD CONSTRAINT cohort_group_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.cohort_membership
  DROP CONSTRAINT cohort_membership_tenant_id_fkey;
ALTER TABLE public.cohort_membership
  ADD CONSTRAINT cohort_membership_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.dokumenty
  DROP CONSTRAINT dokumenty_tenant_id_fkey;
ALTER TABLE public.dokumenty
  ADD CONSTRAINT dokumenty_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.event
  DROP CONSTRAINT event_tenant_id_fkey;
ALTER TABLE public.event
  ADD CONSTRAINT event_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.form_responses
  DROP CONSTRAINT form_responses_tenant_id_fkey;
ALTER TABLE public.form_responses
  ADD CONSTRAINT form_responses_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.galerie_dir
  DROP CONSTRAINT galerie_dir_tenant_id_fkey;
ALTER TABLE public.galerie_dir
  ADD CONSTRAINT galerie_dir_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.galerie_foto
  DROP CONSTRAINT galerie_foto_tenant_id_fkey;
ALTER TABLE public.galerie_foto
  ADD CONSTRAINT galerie_foto_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.location_attachment
  DROP CONSTRAINT location_attachment_location_id_fkey;
ALTER TABLE public.location_attachment
  ADD CONSTRAINT location_attachment_location_id_fkey
  FOREIGN KEY (location_id)
  REFERENCES public.location (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.location_attachment
  DROP CONSTRAINT location_attachment_object_name_fkey;
ALTER TABLE public.location_attachment
  ADD CONSTRAINT location_attachment_object_name_fkey
  FOREIGN KEY (object_name)
  REFERENCES public.attachment (object_name)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.otp_token
  DROP CONSTRAINT otp_token_tenant_id_fkey;
ALTER TABLE public.otp_token
  ADD CONSTRAINT otp_token_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.otp_token
  DROP CONSTRAINT otp_token_user_id_fkey;
ALTER TABLE public.otp_token
  ADD CONSTRAINT otp_token_user_id_fkey
  FOREIGN KEY (user_id)
  REFERENCES public.users (u_id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.platby_category_group
  DROP CONSTRAINT platby_category_group_tenant_id_fkey;
ALTER TABLE public.platby_category_group
  ADD CONSTRAINT platby_category_group_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.platby_category
  DROP CONSTRAINT platby_category_tenant_id_fkey;
ALTER TABLE public.platby_category
  ADD CONSTRAINT platby_category_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.platby_group_skupina
  DROP CONSTRAINT platby_group_skupina_tenant_id_fkey;
ALTER TABLE public.platby_group_skupina
  ADD CONSTRAINT platby_group_skupina_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.platby_group
  DROP CONSTRAINT platby_group_tenant_id_fkey;
ALTER TABLE public.platby_group
  ADD CONSTRAINT platby_group_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.platby_item
  DROP CONSTRAINT platby_item_tenant_id_fkey;
ALTER TABLE public.platby_item
  ADD CONSTRAINT platby_item_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.platby_raw
  DROP CONSTRAINT platby_raw_tenant_id_fkey;
ALTER TABLE public.platby_raw
  ADD CONSTRAINT platby_raw_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.room_attachment
  DROP CONSTRAINT room_attachment_object_name_fkey;
ALTER TABLE public.room_attachment
  ADD CONSTRAINT room_attachment_object_name_fkey
  FOREIGN KEY (object_name)
  REFERENCES public.attachment (object_name)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.room_attachment
  DROP CONSTRAINT room_attachment_room_id_fkey;
ALTER TABLE public.room_attachment
  ADD CONSTRAINT room_attachment_room_id_fkey
  FOREIGN KEY (room_id)
  REFERENCES public.room (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.room
  DROP CONSTRAINT room_location_fkey;
ALTER TABLE public.room
  ADD CONSTRAINT room_location_fkey
  FOREIGN KEY (location)
  REFERENCES public.location (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.skupiny
  DROP CONSTRAINT skupiny_cohort_group_fkey;
ALTER TABLE public.skupiny
  ADD CONSTRAINT skupiny_cohort_group_fkey
  FOREIGN KEY (cohort_group)
  REFERENCES public.cohort_group (id)
  ON DELETE set null;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.skupiny
  DROP CONSTRAINT skupiny_tenant_id_fkey;
ALTER TABLE public.skupiny
  ADD CONSTRAINT skupiny_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.tenant_attachment
  DROP CONSTRAINT tenant_attachment_object_name_fkey;
ALTER TABLE public.tenant_attachment
  ADD CONSTRAINT tenant_attachment_object_name_fkey
  FOREIGN KEY (object_name)
  REFERENCES public.attachment (object_name)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.tenant_attachment
  DROP CONSTRAINT tenant_attachment_tenant_id_fkey;
ALTER TABLE public.tenant_attachment
  ADD CONSTRAINT tenant_attachment_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.tenant_location
  DROP CONSTRAINT tenant_location_location_id_fkey;
ALTER TABLE public.tenant_location
  ADD CONSTRAINT tenant_location_location_id_fkey
  FOREIGN KEY (location_id)
  REFERENCES public.location (id)
  ON DELETE CASCADE;


ALTER TABLE public.tenant_location
  DROP CONSTRAINT tenant_location_tenant_id_fkey;
ALTER TABLE public.tenant_location
  ADD CONSTRAINT tenant_location_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


ALTER TABLE public.upozorneni_skupiny
  DROP CONSTRAINT upozorneni_skupiny_tenant_id_fkey;
ALTER TABLE public.upozorneni_skupiny
  ADD CONSTRAINT upozorneni_skupiny_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


ALTER TABLE public.upozorneni
  DROP CONSTRAINT upozorneni_tenant_id_fkey;
ALTER TABLE public.upozorneni
  ADD CONSTRAINT upozorneni_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


ALTER TABLE public.users
  DROP CONSTRAINT users_tenant_id_fkey;
ALTER TABLE public.users
  ADD CONSTRAINT users_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;
