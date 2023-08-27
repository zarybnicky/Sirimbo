--! Previous: sha1:1bfce4db238e7c9cddd66cf8c5e2ef0486be3dd1
--! Hash: sha1:47aa7cc2f6dfda954dd9e1dd391efe185c9e4fc4

--! split: 1-current.sql
CREATE EXTENSION if not exists btree_gist;

alter table skupiny drop column if exists s_color_text;
alter table event_attendance add column if not exists registration_id bigint not null references event_registration (id) on update cascade on delete cascade;
alter table aktuality add column if not exists title_photo_url text null default null;

alter table couple drop column if exists active;
alter table couple add column if not exists active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL;
create or replace function couple_active(c couple) returns boolean language sql stable as $$
  select now() <@ c.active_range;
$$;
grant all on function couple_active to anonymous;
comment on function couple_active is E'@filterable';

select app_private.drop_policies('public.person');
drop view if exists scoreboard;

alter table tenant_membership drop column if exists active;
alter table tenant_membership add column if not exists active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL;
create or replace function tenant_membership_active(c tenant_membership) returns boolean language sql stable as $$
  select now() <@ c.active_range;
$$;
grant all on function tenant_membership_active to anonymous;
comment on function tenant_membership_active is E'@filterable';

alter table tenant_trainer drop column if exists active;
alter table tenant_trainer add column if not exists active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL;
create or replace function tenant_trainer_active(c tenant_trainer) returns boolean language sql stable as $$
  select now() <@ c.active_range;
$$;
grant all on function tenant_trainer_active to anonymous;
comment on function tenant_trainer_active is E'@filterable';

alter table tenant_administrator drop column if exists active;
alter table tenant_administrator add column if not exists active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL;
create or replace function tenant_administrator_active(c tenant_administrator) returns boolean language sql stable as $$
  select now() <@ c.active_range;
$$;
grant all on function tenant_administrator_active to anonymous;
comment on function tenant_administrator_active is E'@filterable';

create policy admin_all on person to administrator using (true);
create policy view_same_tenant on person for select using (exists (select 1 from tenant_membership where now() <@ active_range and person_id = id and tenant_id in (select my_tenant_ids())));
create policy view_tenant_admin on person for select using (exists (select 1 from tenant_administrator where now() <@ active_range and person_id = id));
create policy view_tenant_trainer on person for select using (exists (select 1 from tenant_trainer where now() <@ active_range and person_id = id));

alter table cohort_membership drop column if exists active;
alter table cohort_membership add column if not exists active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL;
create or replace function cohort_membership_active(c cohort_membership) returns boolean language sql stable as $$
  select now() <@ c.active_range;
$$;
grant all on function cohort_membership_active to anonymous;
comment on function cohort_membership_active is E'@filterable';

comment on column couple.active_range is E'@omit';
comment on column cohort_membership.active_range is E'@omit';
comment on column tenant_membership.active_range is E'@omit';
comment on column tenant_trainer.active_range is E'@omit';
comment on column tenant_administrator.active_range is E'@omit';

CREATE INDEX if not exists couple_range_idx ON public.couple USING gist (active_range);
CREATE INDEX if not exists cohort_membership_range_idx ON public.cohort_membership USING gist (active_range, tenant_id);
CREATE INDEX if not exists tenant_membership_range_idx ON public.tenant_membership USING gist (active_range, tenant_id);
CREATE INDEX if not exists tenant_trainer_range_idx ON public.tenant_trainer USING gist (active_range, tenant_id);
CREATE INDEX if not exists tenant_administrator_range_idx ON public.tenant_administrator USING gist (active_range, tenant_id);

CREATE or replace FUNCTION public.filtered_people(in_tenants bigint[], in_cohort bigint, is_trainer boolean, is_admin boolean) RETURNS SETOF public.person
    LANGUAGE sql STABLE
    AS $$
 select person.* from person
  where
    exists (select 1 from tenant_membership where tenant_id = any (in_tenants) and person_id=person.id and now() <@ active_range)
  and
    case when in_cohort is null then true
    else exists (select 1 from cohort_membership where cohort_id=in_cohort and person_id=person.id and now() <@ active_range) end
  and
    case when is_trainer is null then true
    else is_trainer = exists (select 1 from tenant_trainer where tenant_id = any (in_tenants) and person_id=person.id) end
  and
    case when is_admin is null then true
    else is_admin = exists (select 1 from tenant_administrator where tenant_id = any (in_tenants) and person_id=person.id) end
  order by last_name, first_name
$$;


ALTER TABLE ONLY public.event_attendance
  drop CONSTRAINT if exists event_attendance_unique_event_person_key,
  ADD CONSTRAINT event_attendance_unique_event_person_key UNIQUE (registration_id, instance_id, person_id);
ALTER TABLE ONLY public.event_registration
  drop CONSTRAINT event_registration_unique_event_person_couple_key,
  ADD CONSTRAINT event_registration_unique_event_person_couple_key UNIQUE nulls not distinct (event_id, person_id, couple_id);

create or replace function event_registrants(e event) returns setof person language sql stable as $$
  select person.* from person where id in (
    select unnest(array[person_id, man_id, woman_id]) as id
    from event_registration left join couple on couple.id = couple_id
    where event_id=e.id
  )
$$;
grant all on function event_registrants(event) to anonymous;
comment on function event_registrants is E'@simpleCollections only';

create or replace function app_private.event_registration_person_ids(e event_registration) returns setof bigint language sql as $$
  select e.person_id as id where e.person_id is not null
  union
  select unnest(array[man_id, woman_id]) as id from couple where couple.id = e.couple_id and e.couple_id is not null
$$;

CREATE or replace FUNCTION app_private.tg_event_target_cohort__unregister_members() RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER AS $$
begin
  delete from event_registration where target_cohort_id = OLD.id;
  return OLD;
end;
$$;
select verify_function('app_private.tg_event_target_cohort__unregister_members', 'event_target_cohort');
drop trigger if exists _500_unregister_members on public.event_target_cohort;
CREATE TRIGGER _500_unregister_members
  AFTER DELETE ON public.event_target_cohort
  FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_target_cohort__unregister_members();

CREATE or replace FUNCTION app_private.tg_event_target_cohort__register_members() RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER AS $$
begin
  insert into event_registration (event_id, person_id, target_cohort_id)
  select NEW.event_id, person_id, NEW.id from cohort_membership where cohort_membership.cohort_id = NEW.cohort_id and now() <@ active_range
  on conflict do nothing;
  return NEW;
end;
$$;
select verify_function('app_private.tg_event_target_cohort__register_members', 'event_target_cohort');
drop trigger if exists _500_register_members on public.event_target_cohort;
CREATE TRIGGER _500_register_members
  AFTER INSERT ON public.event_target_cohort
  FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_target_cohort__register_members();

drop trigger if exists _500_delete_attendance on public.event_registration;

CREATE or replace FUNCTION app_private.tg_event_registration__create_attendance() RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER AS $$
begin
  insert into event_attendance (registration_id, instance_id, person_id)
  select NEW.id, event_instance.id, app_private.event_registration_person_ids(NEW) from event_instance where event_id = NEW.event_id
  on conflict do nothing;
  return NEW;
end;
$$;
select verify_function('app_private.tg_event_registration__create_attendance', 'event_registration');
drop trigger if exists _500_create_attendance on public.event_registration;
CREATE TRIGGER _500_create_attendance
  AFTER INSERT ON public.event_registration
  FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_registration__create_attendance();

CREATE or replace FUNCTION app_private.tg_event_instance__create_attendance() RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER AS $$
begin
  insert into event_attendance (registration_id, instance_id, person_id)
  select event_registration.id, NEW.id, app_private.event_registration_person_ids(event_registration) as id
  from event_registration where event_registration.event_id = NEW.event_id
  on conflict do nothing;
  return NEW;
end;
$$;
select verify_function('app_private.tg_event_instance__create_attendance', 'event_instance');
drop trigger if exists _500_create_attendance on public.event_instance;
CREATE TRIGGER _500_create_attendance
  AFTER INSERT ON public.event_instance
  FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_instance__create_attendance();

ALTER TABLE public.aktuality
  DROP CONSTRAINT IF EXISTS aktuality_tenant_id_fkey,
  ADD CONSTRAINT aktuality_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.attendee_user
  DROP CONSTRAINT IF EXISTS attendee_user_tenant_id_fkey,
  ADD CONSTRAINT attendee_user_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.cohort_group
  DROP CONSTRAINT IF EXISTS cohort_group_tenant_id_fkey,
  ADD CONSTRAINT cohort_group_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.cohort_membership
  DROP CONSTRAINT IF EXISTS cohort_membership_tenant_id_fkey,
  ADD CONSTRAINT cohort_membership_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.dokumenty
  DROP CONSTRAINT IF EXISTS dokumenty_tenant_id_fkey,
  ADD CONSTRAINT dokumenty_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.event
  DROP CONSTRAINT IF EXISTS event_tenant_id_fkey,
  ADD CONSTRAINT event_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.form_responses
  DROP CONSTRAINT IF EXISTS form_responses_tenant_id_fkey,
  ADD CONSTRAINT form_responses_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.galerie_dir
  DROP CONSTRAINT IF EXISTS galerie_dir_tenant_id_fkey,
  ADD CONSTRAINT galerie_dir_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.galerie_foto
  DROP CONSTRAINT IF EXISTS galerie_foto_tenant_id_fkey,
  ADD CONSTRAINT galerie_foto_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.nabidka_item
  DROP CONSTRAINT IF EXISTS nabidka_item_tenant_id_fkey,
  ADD CONSTRAINT nabidka_item_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.nabidka
  DROP CONSTRAINT IF EXISTS nabidka_tenant_id_fkey,
  ADD CONSTRAINT nabidka_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.otp_token
  DROP CONSTRAINT IF EXISTS otp_token_tenant_id_fkey,
  ADD CONSTRAINT otp_token_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.person_invitation
  DROP CONSTRAINT IF EXISTS person_invitation_tenant_id_fkey,
  ADD CONSTRAINT person_invitation_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.platby_category_group
  DROP CONSTRAINT IF EXISTS platby_category_group_tenant_id_fkey,
  ADD CONSTRAINT platby_category_group_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.platby_category
  DROP CONSTRAINT IF EXISTS platby_category_tenant_id_fkey,
  ADD CONSTRAINT platby_category_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.platby_group_skupina
  DROP CONSTRAINT IF EXISTS platby_group_skupina_tenant_id_fkey,
  ADD CONSTRAINT platby_group_skupina_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.platby_group
  DROP CONSTRAINT IF EXISTS platby_group_tenant_id_fkey,
  ADD CONSTRAINT platby_group_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.platby_item
  DROP CONSTRAINT IF EXISTS platby_item_tenant_id_fkey,
  ADD CONSTRAINT platby_item_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.platby_raw
  DROP CONSTRAINT IF EXISTS platby_raw_tenant_id_fkey,
  ADD CONSTRAINT platby_raw_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.rozpis_item
  DROP CONSTRAINT IF EXISTS rozpis_item_tenant_id_fkey,
  ADD CONSTRAINT rozpis_item_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.rozpis
  DROP CONSTRAINT IF EXISTS rozpis_tenant_id_fkey,
  ADD CONSTRAINT rozpis_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.skupiny
  DROP CONSTRAINT IF EXISTS skupiny_tenant_id_fkey,
  ADD CONSTRAINT skupiny_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.upozorneni_skupiny
  DROP CONSTRAINT IF EXISTS upozorneni_skupiny_tenant_id_fkey,
  ADD CONSTRAINT upozorneni_skupiny_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.upozorneni
  DROP CONSTRAINT IF EXISTS upozorneni_tenant_id_fkey,
  ADD CONSTRAINT upozorneni_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;
ALTER TABLE public.users
  DROP CONSTRAINT IF EXISTS users_tenant_id_fkey,
  ADD CONSTRAINT users_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant;

alter table cohort_group drop column if exists tenant;

CREATE INDEX IF NOT EXISTS idx_cg_tenant ON "public"."cohort_group"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_cm_tenant ON "public"."cohort_membership"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_d_tenant ON "public"."dokumenty"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_e_tenant ON "public"."event"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_fr_tenant ON "public"."form_responses"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_gd_tenant ON "public"."galerie_dir"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_gf_tenant ON "public"."galerie_foto"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_pc_tenant ON "public"."platby_category"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_pi_tenant ON "public"."platby_item"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_sk_tenant ON "public"."skupiny"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_up_tenant ON "public"."upozorneni"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_us_tenant ON "public"."users"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_au_tenant ON "public"."attendee_user"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_na_tenant ON "public"."nabidka"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_ni_tenant ON "public"."nabidka_item"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_ot_tenant ON "public"."otp_token"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_pei_tenant ON "public"."person_invitation"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_pcg_tenant ON "public"."platby_category_group"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_pg_tenant ON "public"."platby_group"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_pgs_tenant ON "public"."platby_group_skupina"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_pr_tenant ON "public"."platby_raw"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_ro_tenant ON "public"."rozpis"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_ri_tenant ON "public"."rozpis_item"("tenant_id");
CREATE INDEX IF NOT EXISTS idx_ups_tenant ON "public"."upozorneni_skupiny"("tenant_id");

-- insert into event_target_cohort (tenant_id, event_id, cohort_id)
-- select r.tenant_id, r.event_id, 2 from event_registration r where person_id = 152;

-- delete from event_registration where person_id = 152;

-- 1) list ids to migrate. User ID + Cohort ID, "Everybody" Cohort IDs
-- 2) create event_target_cohort for all registrations in ^^
-- 3) create nested event_registration with link
-- 4) delete original cohort registrations

-- DONE
-- person 316 = zelená (skupiny = 14)
-- person 346 = fialová (skupiny = 11)
-- person 315 = modrá (skupiny = 5)
-- person 207 = červená (skupiny = 4)
-- person 152 = žlutá skupina (skupiny = 2)

-- TODO
-- person 208 = společná            ...od 2015, 214x
-- person 209 = performance         ...od 2015, 87x
-- person 210 = practice            ...od 2015, 110x
-- person 155 = "společná lekce"    ...od 2023, 5x
-- person 642 = pohybovka           ...od 2023, pouze terka florová


-- 5) delete registrations with target_cohort_id where user was not yet a member of the cohort
-- with regs_and_cohorts as (
--  select event_registration.id as id, event.since as event_since, cohort_membership.since as member_since
--  from event_registration
--  inner join event on event_registration.event_id=event.id
--  left join couple on event_registration.couple_id=couple.id
--  inner join event_target_cohort on target_cohort_id=event_target_cohort.id
--  inner join skupiny on cohort_id=skupiny.id
--  inner join cohort_membership on skupiny.id=cohort_membership.cohort_id and cohort_membership.person_id in (event_registration.person_id, couple.man_id, couple.woman_id)
--  where target_cohort_id is not null
--  and event.since < cohort_membership.since
-- )
-- delete from event_registration where event_registration.id in (select id from regs_and_cohorts);

-- insert into event_attendance (registration_id, instance_id, person_id)
-- select event_registration.id, event_instance.id, app_private.event_registration_person_ids(event_registration)
-- from event_registration inner join event_instance on event_instance.event_id=event_registration.event_id on conflict do nothing;

-- 6) add scoreboard view
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
    and event.type = 'lesson'
    and event_instance.since > '2022-01-01T00:00:00.0000Z'
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

CREATE or replace FUNCTION person_couple_ids(p person) RETURNS bigint[] LANGUAGE sql STABLE AS $$
  select array_agg(couple.id)
  from couple
  where man_id = p.id or woman_id = p.id and now() <@ active_range;
$$;
GRANT ALL ON FUNCTION person_couple_ids TO anonymous;

CREATE or replace FUNCTION my_couple_ids() RETURNS SETOF bigint LANGUAGE sql STABLE security definer AS $$
  select couple.id
  from couple join person on (man_id = person.id or woman_id = person.id) join user_proxy on person_id=person.id
  where user_id = current_user_id() and now() <@ active_range;
$$;
GRANT ALL ON FUNCTION my_couple_ids() TO anonymous;
comment on function my_couple_ids is '@omit';

CREATE or replace FUNCTION person_tenant_ids(p person) RETURNS bigint[] LANGUAGE sql STABLE AS $$
  select array_agg(tenant_id) from tenant_membership where now() <@ active_range and person_id = p.id;
$$;
GRANT ALL ON FUNCTION person_tenant_ids TO anonymous;

CREATE or replace FUNCTION person_cohort_ids(p person) RETURNS bigint[] LANGUAGE sql STABLE AS $$
  select array_agg(cohort_id) from cohort_membership where now() <@ active_range and person_id = p.id;
$$;
GRANT ALL ON FUNCTION person_cohort_ids TO anonymous;

CREATE or replace FUNCTION my_tenant_ids() RETURNS SETOF bigint LANGUAGE sql STABLE security definer AS $$
  select tenant.id
  from tenant join tenant_membership on tenant.id = tenant_id
  where now() <@ tenant_membership.active_range and person_id in (select my_person_ids());
$$;
GRANT ALL ON FUNCTION my_tenant_ids() TO anonymous;
comment on function my_tenant_ids is '@omit';

CREATE or replace FUNCTION public.person_couples(p person) RETURNS SETOF public.couple LANGUAGE sql STABLE AS $$
  select couple.* from couple where (man_id = p.id or woman_id = p.id) and now() <@ active_range;
$$;
GRANT ALL ON FUNCTION public.person_couples(person) TO anonymous;
comment on function person_couples is E'@simpleCollections only';

CREATE or replace FUNCTION person_is_admin(p person) RETURNS boolean LANGUAGE sql STABLE AS $$
  select exists (select 1 from tenant_administrator where now() <@ active_range and person_id = p.id);
$$;
GRANT ALL ON FUNCTION person_is_admin TO anonymous;
CREATE or replace FUNCTION person_is_trainer(p person) RETURNS boolean LANGUAGE sql STABLE AS $$
  select exists (select 1 from tenant_trainer where now() <@ active_range and person_id = p.id);
$$;
GRANT ALL ON FUNCTION person_is_admin TO anonymous;
