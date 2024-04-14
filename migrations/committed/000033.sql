--! Previous: sha1:be3ff845e7aa833447272d37e6eb53c4d08173b6
--! Hash: sha1:872ef97cb54f51e3c3e1f71e74c7248935e951cb

--! split: 30-created-updated-at.sql
do $$
begin
  drop trigger if exists on_update_current_timestamp on upozorneni;
  drop function if exists on_update_current_timestamp_upozorneni;
  if not exists (select 1 from information_schema.columns where table_schema = 'public' and table_name = 'upozorneni' and column_name = 'created_at') then
    alter table upozorneni rename column up_timestamp_add to created_at;
    alter table upozorneni rename column up_timestamp to updated_at;
  end if;
  alter table upozorneni
      add column if not exists up_timestamp timestamptz not null generated always as (updated_at) stored,
      add column if not exists up_timestamp_add timestamptz not null generated always as (created_at) stored;
  drop trigger if exists _100_timestamps ON upozorneni;
  CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON upozorneni FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

  drop trigger if exists on_update_current_timestamp on dokumenty;
  drop function if exists on_update_current_timestamp_dokumenty;
  if not exists (select 1 from information_schema.columns where table_schema = 'public' and table_name = 'dokumenty' and column_name = 'updated_at') then
    alter table dokumenty rename column d_timestamp to updated_at;
  end if;
  alter table dokumenty
      add column if not exists created_at timestamptz default current_timestamp,
      add column if not exists d_timestamp timestamptz generated always as (updated_at) stored;
  drop trigger if exists _100_timestamps ON dokumenty;
  CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON dokumenty FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

  drop trigger if exists on_update_current_timestamp on aktuality;
  drop function if exists on_update_current_timestamp_aktuality;
  if not exists (select 1 from information_schema.columns where table_schema = 'public' and table_name = 'aktuality' and column_name = 'created_at') then
    alter table aktuality rename column at_timestamp_add to created_at;
    alter table aktuality rename column at_timestamp to updated_at;
  end if;
  alter table aktuality
      add column if not exists at_timestamp timestamptz not null generated always as (updated_at) stored,
      add column if not exists at_timestamp_add timestamptz not null generated always as (created_at) stored;
  drop trigger if exists _100_timestamps ON aktuality;
  CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON aktuality FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

  drop trigger if exists on_update_current_timestamp on galerie_foto;
  drop function if exists on_update_current_timestamp_galerie_foto;
  if not exists (select 1 from information_schema.columns where table_schema = 'public' and table_name = 'galerie_foto' and column_name = 'updated_at') then
    alter table galerie_foto rename column gf_timestamp to updated_at;
  end if;
  alter table galerie_foto
      add column if not exists created_at timestamptz default current_timestamp,
      add column if not exists gf_timestamp timestamptz generated always as (updated_at) stored;
  drop trigger if exists _100_timestamps ON galerie_foto;
  CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON galerie_foto FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

  drop trigger if exists on_update_current_timestamp on users;
  drop function if exists on_update_current_timestamp_users;
  if not exists (select 1 from information_schema.columns where table_schema = 'public' and table_name = 'users' and column_name = 'updated_at') then
    alter table users rename column u_timestamp to updated_at;
  end if;
  alter table users
      add column if not exists created_at timestamptz default current_timestamp,
      add column if not exists u_timestamp timestamptz generated always as (updated_at) stored;
  drop trigger if exists _100_timestamps ON users;
  CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON users FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

  drop trigger if exists on_update_event_timestamp on event;
  drop function if exists on_update_event_timestamp;
  alter table event
      add column if not exists created_at timestamptz default current_timestamp;
  drop trigger if exists _100_timestamps ON event;
  CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON event FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
end;
$$;

--! split: 40-trainer-editable-events.sql
create or replace function app_private.can_trainer_edit_event(eid bigint) returns boolean language sql as $$
  select (
    select count(person_id) > 0 from event_trainer where eid = event_id and person_id = any (my_persons_array())
  ) or not exists (
    select 1 from event_trainer where eid = event_id
  );
$$ security definer;
grant all on function app_private.can_trainer_edit_event to anonymous;


select app_private.drop_policies('public.event');
CREATE POLICY my_tenant ON public.event AS RESTRICTIVE USING (tenant_id = current_tenant_id());
CREATE POLICY admin_same_tenant ON public.event to administrator USING (tenant_id = any (my_tenants_array()));
CREATE POLICY trainer_same_tenant ON public.event to trainer
  USING (app_private.can_trainer_edit_event(id))
  WITH CHECK (tenant_id = any (my_tenants_array()));
CREATE POLICY view_public ON public.event FOR SELECT TO anonymous USING (is_public = true or tenant_id = any (my_tenants_array()));

select app_private.drop_policies('public.event_instance');
CREATE POLICY my_tenant ON public.event_instance AS RESTRICTIVE USING (tenant_id = current_tenant_id());
CREATE POLICY admin_same_tenant ON public.event_instance to administrator USING (tenant_id = any (my_tenants_array()));
CREATE POLICY trainer_same_tenant ON public.event_instance to trainer
  USING (app_private.can_trainer_edit_event(event_id))
  WITH CHECK (tenant_id = any (my_tenants_array()));
CREATE POLICY view_visible_event ON public.event_instance FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event
  WHERE (event_instance.event_id = event.id))));

select app_private.drop_policies('public.event_trainer');
CREATE POLICY my_tenant ON public.event_trainer AS RESTRICTIVE USING (tenant_id = current_tenant_id());
create policy admin_all on event_trainer to administrator using (true);
CREATE POLICY trainer_same_tenant ON public.event_trainer to trainer
  USING (app_private.can_trainer_edit_event(event_id))
  WITH CHECK (tenant_id = any (my_tenants_array()));
create policy view_all on event_trainer for select to member using (true);

select app_private.drop_policies('public.event_target_cohort');
create policy my_tenant on event_target_cohort as restrictive using (tenant_id = current_tenant_id());
create policy admin_all on event_target_cohort to administrator using (true);
CREATE POLICY trainer_same_tenant ON public.event_target_cohort to trainer
  USING (app_private.can_trainer_edit_event(event_id))
  WITH CHECK (tenant_id = any (my_tenants_array()));
create policy view_tenant on event_target_cohort for select to member using (true);

select app_private.drop_policies('public.event_instance_trainer');
create policy my_tenant on event_instance_trainer as restrictive using (tenant_id = current_tenant_id());
create policy admin_all on event_instance_trainer to administrator using (true);
CREATE POLICY trainer_same_tenant ON public.event_instance_trainer to trainer
  USING (app_private.can_trainer_edit_event((select event_id from event_instance i where i.id = instance_id)))
  WITH CHECK (tenant_id = any (my_tenants_array()));
create policy view_tenant on event_instance_trainer for select to member using (true);

select app_private.drop_policies('public.event_registration');
create policy admin_all on event_registration to administrator using (true);
CREATE POLICY trainer_same_tenant ON public.event_registration to trainer
  USING (app_private.can_trainer_edit_event(event_id))
  WITH CHECK (tenant_id = any (my_tenants_array()));
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

--! split: 50-refactor-ids.sql
alter table aktuality
  alter column id set not null,
  drop constraint if exists aktuality_unique_id,
  add constraint aktuality_unique_id unique (id);
alter table dokumenty
  alter column id set not null,
  drop constraint if exists dokumenty_unique_id,
  add constraint dokumenty_unique_id unique (id);
alter table galerie_dir
  alter column id set not null,
  drop constraint if exists galerie_dir_unique_id,
  add constraint galerie_dir_unique_id unique (id);
alter table galerie_foto
  alter column id set not null,
  drop constraint if exists galerie_foto_unique_id,
  add constraint galerie_foto_unique_id unique (id);
alter table platby_category
  alter column id set not null,
  drop constraint if exists platby_category_unique_id,
  add constraint platby_category_unique_id unique (id);
alter table platby_category_group
  alter column id set not null,
  drop constraint if exists platby_category_group_unique_id,
  add constraint platby_category_group_unique_id unique (id);
alter table platby_group
  alter column id set not null,
  drop constraint if exists platby_group_unique_id,
  add constraint platby_group_unique_id unique (id);
alter table platby_group_skupina
  alter column id set not null,
  drop constraint if exists platby_group_skupina_unique_id,
  add constraint platby_group_skupina_unique_id unique (id);
alter table platby_item
  alter column id set not null,
  drop constraint if exists platby_item_unique_id,
  add constraint platby_item_unique_id unique (id);
alter table platby_raw
  alter column id set not null,
  drop constraint if exists platby_raw_unique_id,
  add constraint platby_raw_unique_id unique (id);
alter table upozorneni_skupiny
  alter column id set not null,
  drop constraint if exists upozorneni_skupiny_unique_id,
  add constraint upozorneni_skupiny_unique_id unique (id);
alter table users
  alter column id set not null,
  drop constraint if exists users_unique_id,
  add constraint users_unique_id unique (id);
alter table upozorneni
  alter column id set not null,
  drop constraint if exists upozorneni_unique_id,
  add constraint upozorneni_unique_id unique (id);
alter table skupiny
  alter column id set not null,
  drop constraint if exists skupiny_unique_id,
  add constraint skupiny_unique_id unique (id);
alter table app_private.pary_navrh
  alter column id set not null,
  drop constraint if exists pary_navrh_unique_id,
  add constraint pary_navrh_unique_id unique (id);

--! split: 60-auto-memberships.sql
do $$
begin
  if not exists (select * from pg_type where typcategory='E' and typname = 'relationship_status') then
    create type relationship_status as enum (
      'pending',
      'active',
      'expired'
    );
  end if;
end
$$;
COMMENT ON TABLE public.posting IS E'@omit create,update,delete
@simpleCollections both';

alter table user_proxy add column if not exists status relationship_status not null default 'active';
alter table couple add column if not exists status relationship_status not null default 'active';
alter table cohort_membership add column if not exists status relationship_status not null default 'active';
alter table tenant_membership add column if not exists status relationship_status not null default 'active';
alter table tenant_trainer add column if not exists status relationship_status not null default 'active';
alter table tenant_administrator add column if not exists status relationship_status not null default 'active';

drop function if exists user_proxy_active;
drop function if exists couple_active;
drop function if exists cohort_membership_active;
drop function if exists tenant_membership_active;
drop function if exists tenant_trainer_active;
drop function if exists tenant_administrator_active;
alter table user_proxy add column if not exists active boolean not null generated always as (status = 'active') stored;
alter table couple add column if not exists active boolean not null generated always as (status = 'active') stored;
alter table cohort_membership add column if not exists active boolean not null generated always as (status = 'active') stored;
alter table tenant_membership add column if not exists active boolean not null generated always as (status = 'active') stored;
alter table tenant_trainer add column if not exists active boolean not null generated always as (status = 'active') stored;
alter table tenant_administrator add column if not exists active boolean not null generated always as (status = 'active') stored;

create index if not exists user_proxy_status_idx on user_proxy (status);
create index if not exists couple_status_idx on couple (status);
create index if not exists cohort_membership_status_idx on cohort_membership (status);
create index if not exists tenant_membership_status_idx on tenant_membership (status);
create index if not exists tenant_trainer_status_idx on tenant_trainer (status);
create index if not exists tenant_administrator_status_idx on tenant_administrator (status);

create index if not exists user_proxy_active_idx on user_proxy (active);
create index if not exists couple_active_idx on couple (active);
create index if not exists cohort_membership_active_idx on cohort_membership (active);
create index if not exists tenant_membership_active_idx on tenant_membership (active);
create index if not exists tenant_trainer_active_idx on tenant_trainer (active);
create index if not exists tenant_administrator_active_idx on tenant_administrator (active);

create or replace function app_private.cron_update_memberships() returns void language sql as $$
  update user_proxy set status = 'active' where now() <@ active_range and status <> 'active';
  update user_proxy set status = 'pending' where now() < since and status <> 'pending';
  update user_proxy set status = 'expired' where now() > until and status <> 'expired';

  update couple set status = 'active' where now() <@ active_range and status <> 'active';
  update couple set status = 'pending' where now() < since and status <> 'pending';
  update couple set status = 'expired' where now() > until and status <> 'expired';

  update cohort_membership set status = 'active' where now() <@ active_range and status <> 'active';
  update cohort_membership set status = 'pending' where now() < since and status <> 'pending';
  update cohort_membership set status = 'expired' where now() > until and status <> 'expired';

  update tenant_membership set status = 'active' where now() <@ active_range and status <> 'active';
  update tenant_membership set status = 'pending' where now() < since and status <> 'pending';
  update tenant_membership set status = 'expired' where now() > until and status <> 'expired';

  update tenant_trainer set status = 'active' where now() <@ active_range and status <> 'active';
  update tenant_trainer set status = 'pending' where now() < since and status <> 'pending';
  update tenant_trainer set status = 'expired' where now() > until and status <> 'expired';

  update tenant_administrator set status = 'active' where now() <@ active_range and status <> 'active';
  update tenant_administrator set status = 'pending' where now() < since and status <> 'pending';
  update tenant_administrator set status = 'expired' where now() > until and status <> 'expired';
$$;
grant all on function app_private.cron_update_memberships to administrator;

do $$
declare
  id bigint;
begin
  if exists (select 1 from pg_extension where extname='pg_cron') then
    select jobid into id from cron.job where jobname = 'refresh auth_details';
    if found then
      perform cron.unschedule(id);
    end if;

    perform cron.schedule('update memberships', '59 seconds', 'select app_private.cron_update_memberships();');
  end if;
end
$$;

create or replace function app_private.tg_tenant_membership__on_status() returns trigger language plpgsql as $$
begin
  if NEW.status = 'expired' then
    update cohort_membership set status = 'expired', until = NEW.until where cohort_membership.person_id = NEW.person_id;
  end if;
  return NEW;
end;
$$;
select verify_function('app_private.tg_tenant_membership__on_status', 'tenant_membership');

CREATE or replace TRIGGER _500_on_status
  after UPDATE ON tenant_membership
  FOR EACH row
  WHEN (OLD.status IS DISTINCT FROM NEW.status)
  EXECUTE PROCEDURE app_private.tg_tenant_membership__on_status();

create or replace function app_private.tg_cohort_membership__on_status() returns trigger language plpgsql as $$
begin
  if NEW.status = 'expired' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then
    -- TODO: remove event_registrations for future events
    -- remove event_attendance for ongoing events
  elsif NEW.status = 'active' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then
    -- add payments
    -- add event_registrations to cohort events
  end if;
  return NEW;
end;
$$;
select verify_function('app_private.tg_cohort_membership__on_status', 'cohort_membership');

CREATE or replace TRIGGER _500_on_status
  after insert or UPDATE ON cohort_membership
  FOR EACH row
  EXECUTE PROCEDURE app_private.tg_cohort_membership__on_status();

--! split: 61-use-active-status.sql
drop function if exists app_private.tg__person_email_primary;
drop function if exists app_private.tg__person_address_primary;
drop function if exists app_private.tg__person_phone_primary;
drop function if exists public.users_in_public_cohort;
drop function if exists users_date_of_oldest_payment;
drop function if exists users_date_of_newest_payment;
drop function if exists on_update_current_timestamp_rozpis;
drop function if exists on_update_current_timestamp_nabidka;
drop function if exists person_couple_ids;

comment on function skupiny_in_current_tenant is E'@filterable
@deprecated';

alter table event_registration
    alter is_confirmed set default false,
    alter confirmed_at set default null;
drop function if exists is_current_tenant_member;

create or replace view scoreboard as
  with members as (
    select person.id
    from person
    inner join cohort_membership on cohort_membership.person_id=person.id
    where cohort_membership.active and tenant_id=current_tenant_id()
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
    and not i.is_cancelled
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

create or replace function tenant_couples(t tenant) returns setof couple stable
begin atomic
  select distinct couple.*
  from couple
  join tenant_membership on man_id=person_id or woman_id=person_id
  where couple.active and tenant_membership.active and tenant_id=t.id
  order by couple.active_range asc;
end;
grant all on function tenant_couples to anonymous;
comment on function tenant_couples is '@simpleCollections only';

CREATE or replace FUNCTION public.person_active_couples(p public.person) RETURNS SETOF public.couple STABLE
begin atomic
  select * from couple where (man_id = p.id or woman_id = p.id) and active order by active_range;
end;
CREATE or replace FUNCTION public.person_all_couples(p public.person) RETURNS SETOF public.couple STABLE
begin atomic
  select * from couple where (man_id = p.id or woman_id = p.id) order by active_range;
end;

--! split: 70-password-reset.sql
alter table otp_token drop column if exists intent;

drop function if exists reset_password;
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
    values (v_user.u_id) returning * into v_token;

    select jsonb_agg(person_name(person.*)) into v_people
    from user_proxy join person on person_id=person.id
    where active and user_id = v_user.u_id;

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

select verify_function('reset_password');
GRANT ALL ON FUNCTION reset_password TO anonymous;

drop function if exists otp_login;
CREATE or replace FUNCTION public.otp_login(token uuid, OUT usr public.users, OUT jwt public.jwt_token) RETURNS record
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  v_token otp_token;
begin
  select * into v_token from otp_token where access_token = token and used_at is null and expires_at > now();
  if not found then
    raise exception 'INVALID_CREDENTIALS' using errcode = '28P01';
  end if;
  select * into usr from users where u_id = v_token.user_id;

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

-- select verify_function('otp_login');
GRANT ALL ON FUNCTION otp_login TO anonymous;
