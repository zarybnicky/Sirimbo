--! Previous: sha1:1748ccce91866980da17ae8e6c992182ea57aac2
--! Hash: sha1:29ccef0ffa32dc2dae6f9d6e023c01f57337a0e2

--! split: 001-cleanup.sql
drop function if exists title_videos();
drop function if exists app_private.tg__tenant();
drop table if exists app_private.video_source;
drop table if exists app_private.video_list;
drop table if exists app_private.video;
drop table if exists public.page_revision;
drop table if exists public.page;
drop table if exists app_private.page_revision;
drop table if exists app_private.page;

UPDATE pary SET p_id_partnerka=NULL where p_id_partnerka=0;
DELETE FROM pary WHERE p_id_partnerka = 29;
DELETE FROM pary WHERE p_id_partnerka = 33;
DELETE FROM pary WHERE p_id_partnerka = 83;
DELETE FROM pary WHERE p_id_partnerka = 104;
DELETE FROM pary WHERE p_id_partnerka = 132;
DELETE FROM pary WHERE p_id_partnerka = 147;
ALTER TABLE pary DROP CONSTRAINT IF EXISTS pary_p_id_partnerka_fkey;
ALTER TABLE pary ADD CONSTRAINT pary_p_id_partnerka_fkey FOREIGN KEY (p_id_partnerka) REFERENCES users (u_id) ON DELETE CASCADE ON UPDATE CASCADE;

comment on table pary is E'@omit create,update,delete';
comment on table users is E'@omit create,update,delete';
comment on table rozpis is E'@omit create,update,delete';
comment on table rozpis_item is E'@omit create,update,delete';
comment on table nabidka is E'@omit create,update,delete';
comment on table nabidka_item is E'@omit create,update,delete';
comment on table galerie_dir is E'@omit create,update,delete';
comment on table galerie_foto is E'@omit create,update,delete';
comment on table permissions is E'@omit create,update,delete';
comment on table parameters is E'@omit create,update,delete';
comment on table pary_navrh is E'@omit create,update,delete';
comment on table tenant is E'@omit create,delete';
comment on table form_responses is E'@omit update,delete';

drop function if exists delete_couple(id bigint);
CREATE or replace FUNCTION public.delete_couple(couple_id bigint) RETURNS SETOF public.pary LANGUAGE plpgsql STRICT SECURITY DEFINER AS $$
declare
  couple pary;
begin
  update pary set p_archiv = true where p_id = couple_id returning * into couple;

  insert into pary (p_id_partner, p_id_partnerka) VALUES (couple.p_id_partner, null);
  if couple.p_id_partnerka is not null and couple.p_id_partnerka <> 0 then
    insert into pary (p_id_partner, p_id_partnerka) VALUES (couple.p_id_partner, null);
  end if;
  return next couple;
end;
$$;
select verify_function('delete_couple');
GRANT ALL ON FUNCTION public.delete_couple(bigint) TO administrator;


create or replace function legacy_duplicate_nabidka(nabidka_id bigint) RETURNS public.nabidka LANGUAGE plpgsql STRICT as $$
declare
  new nabidka;
begin
  INSERT INTO nabidka (n_trener, n_pocet_hod, n_max_pocet_hod, n_od, n_do, n_visible, n_lock)
  SELECT n_trener, n_pocet_hod, n_max_pocet_hod, n_od, n_do, n_visible, n_lock FROM nabidka WHERE n_id=nabidka_id
  RETURNING * into new;

  INSERT INTO nabidka_item (ni_partner, ni_id_rodic, ni_pocet_hod)
  SELECT ni_partner, new.id, ni_pocet_hod FROM nabidka_item WHERE ni_id_rodic = nabidka_id;

  RETURN new;
end;
$$;
select verify_function('legacy_duplicate_nabidka');
GRANT ALL ON FUNCTION public.legacy_duplicate_nabidka(bigint) TO anonymous;

create or replace function legacy_duplicate_rozpis(rozpis_id bigint) RETURNS public.rozpis LANGUAGE plpgsql STRICT as $$
declare
  new rozpis;
begin
  INSERT INTO rozpis (r_trener, r_datum, r_kde, r_visible, r_lock)
  SELECT r_trener, r_datum, r_kde, r_visible, r_lock FROM rozpis WHERE r_id=rozpis_id
  RETURNING * into new;

  INSERT INTO rozpis_item (ri_id_rodic, ri_partner, ri_od, ri_do, ri_lock)
  SELECT new.id, ri_partner, ri_od, ri_do, ri_lock FROM rozpis_item WHERE ri_id_rodic = rozpis_id;

  RETURN new;
end;
$$;
select verify_function('legacy_duplicate_rozpis');
GRANT ALL ON FUNCTION public.legacy_duplicate_rozpis(bigint) TO anonymous;

--! split: 002-person.sql
/*
ADDRESS - DONE
u_street text NOT NULL,
u_conscription_number text DEFAULT ''::text NOT NULL,
u_orientation_number text DEFAULT ''::text NOT NULL,
u_district text DEFAULT ''::text NOT NULL,
u_city text NOT NULL,
u_postal_code text NOT NULL,

USER - DONE
u_login text NOT NULL,
u_pass character(40) NOT NULL,
u_timestamp timestamp with time zone DEFAULT now() NOT NULL,
last_login
password_attempts, last_password_change
u_gdpr_signed_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,

PROXY - DONE
user_id
person_id
u_member_since timestamptz,
u_member_until timestamptz,

COUPLE - DONE
man_id
woman_id
u_member_since timestamptz,
u_member_until timestamptz,

COUPLE_POINTS_SNAPSHOT
(ČSTS + ranklist, WDSF + ranklist)

COUPLE_COMPETITION
???

PERSON - DONE
u_jmeno text NOT NULL,
u_prijmeni text NOT NULL,
u_pohlavi text NOT NULL,
u_email text NOT NULL,
u_telefon text NOT NULL,
u_narozeni date NOT NULL,
u_rodne_cislo text,
u_nationality text NOT NULL,
u_timestamp timestamp with time zone DEFAULT now() NOT NULL,
u_created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,

TENANT_MEMBERSHIP - DONE
u_member_since timestamptz,
u_member_until timestamptz,

TENANT_MEMBERSHIP_APPLICATION
u_confirmed boolean DEFAULT false NOT NULL,
u_poznamky text DEFAULT ''::text NOT NULL,
u_skupina bigint DEFAULT '1'::bigint NOT NULL,
u_dancer boolean DEFAULT true NOT NULL,

COHORT_MEMBERSHIP - DONE
u_skupina bigint DEFAULT '1'::bigint NOT NULL,
u_member_since timestamptz,
u_member_until timestamptz,

TENANT_TEACHER - DONE
u_teacher boolean DEFAULT false NOT NULL,
u_member_since timestamptz,
u_member_until timestamptz,

TENANT_ADMINISTRATOR - DONE
u_group bigint DEFAULT 0 NOT NULL,
u_member_since timestamptz,
u_member_until timestamptz,

u_level smallint DEFAULT '0'::smallint NOT NULL,
u_lock boolean DEFAULT true NOT NULL,
u_system boolean DEFAULT true NOT NULL,
u_ban boolean DEFAULT true NOT NULL,
*/

DROP TABLE if exists address cascade;
DROP TABLE if exists person cascade;
DROP TABLE if exists person_address cascade;
DROP TABLE if exists person_email cascade;
DROP TABLE if exists person_phone cascade;
DROP TABLE if exists user_proxy cascade;
DROP TABLE if exists tenant_membership cascade;
DROP TABLE if exists tenant_trainer cascade;
DROP TABLE if exists tenant_administrator cascade;
DROP TABLE if exists cohort_membership cascade;
DROP TABLE if exists couple cascade;

drop type if exists gender_type;
create type gender_type as enum (
  'man',
  'woman',
  'unspecified'
);

CREATE TABLE person (
  id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  first_name text not null,
  middle_name text default null,
  last_name text not null,
  gender gender_type not null,
  birth_date date not null,
  nationality text not null,
  tax_identification_number text default null,
  national_id_number text default null,
  csts_id text default null,
  wdsf_id text default null,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  legacy_user_id bigint null
);
create trigger _100_timestamps before insert or update on person for each row execute procedure app_private.tg__timestamps();
comment on table person is E'@omit create,update,delete';
GRANT ALL ON TABLE person TO anonymous;
ALTER TABLE person ENABLE ROW LEVEL SECURITY;

CREATE TABLE address (
  id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  street text NOT NULL,
  conscription_number text not null DEFAULT ''::text,
  orientation_number text not null DEFAULT ''::text,
  district text not null DEFAULT ''::text,
  city text NOT NULL,
  postal_code text NOT NULL,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);
create trigger _100_timestamps before insert or update on address for each row execute procedure app_private.tg__timestamps();
comment on table address is E'@omit create,update,delete';
GRANT ALL ON TABLE address TO anonymous;
ALTER TABLE address ENABLE ROW LEVEL SECURITY;

CREATE TABLE person_address (
  person_id bigint not null references person (id) ON UPDATE CASCADE ON DELETE CASCADE,
  address_id bigint not null references address (id) ON UPDATE CASCADE ON DELETE CASCADE,
  is_primary boolean default false,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  PRIMARY KEY (person_id, address_id)
);
create trigger _100_timestamps before insert or update on person_address for each row execute procedure app_private.tg__timestamps();
comment on table person_address is E'@omit create,update,delete';
GRANT ALL ON TABLE person_address TO anonymous;
ALTER TABLE person_address ENABLE ROW LEVEL SECURITY;
CREATE INDEX ON person_address(address_id);
CREATE INDEX ON person_address(person_id);

create or replace function app_private.tg__person_address_primary() returns trigger language plpgsql as $$
begin
  if not exists (select * from person_address where person_address.person_id = person_id) then
    NEW.is_primary = true;
  end if;
  return new;
end;
$$;
select verify_function('app_private.tg__person_address_primary', 'person_address');
CREATE TRIGGER _200_primary BEFORE INSERT OR UPDATE ON person_address
  FOR EACH ROW EXECUTE FUNCTION app_private.tg__person_address_primary();

CREATE TABLE person_email (
  person_id bigint not null references person (id),
  email text not null,
  is_primary boolean default false,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  PRIMARY KEY (person_id, email)
);
create trigger _100_timestamps before insert or update on person_email for each row execute procedure app_private.tg__timestamps();
comment on table person_email is E'@omit create,update,delete';
GRANT ALL ON TABLE person_email TO anonymous;
ALTER TABLE person_email ENABLE ROW LEVEL SECURITY;
CREATE INDEX ON person_email(person_id);

create or replace function app_private.tg__person_email_primary() returns trigger language plpgsql as $$
begin
  if not exists (select * from person_email where person_email.person_id = person_id) then
    NEW.is_primary = true;
  end if;
  return new;
end;
$$;
select verify_function('app_private.tg__person_email_primary', 'person_email');
CREATE TRIGGER _200_primary BEFORE INSERT OR UPDATE ON person_email
  FOR EACH ROW EXECUTE FUNCTION app_private.tg__person_email_primary();

CREATE TABLE person_phone (
  person_id bigint not null references person (id) ON UPDATE CASCADE ON DELETE CASCADE,
  phone text not null,
  is_primary boolean default false,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  PRIMARY KEY (person_id, phone)
);
create trigger _100_timestamps before insert or update on person_phone for each row execute procedure app_private.tg__timestamps();
comment on table person_phone is E'@omit create,update,delete';
GRANT ALL ON TABLE person_phone TO anonymous;
ALTER TABLE person_phone ENABLE ROW LEVEL SECURITY;
CREATE INDEX ON person_phone(person_id);

create or replace function app_private.tg__person_phone_primary() returns trigger language plpgsql as $$
begin
  if not exists (select * from person_phone where person_phone.person_id = person_id) then
    NEW.is_primary = true;
  end if;
  return new;
end;
$$;
select verify_function('app_private.tg__person_phone_primary', 'person_phone');
CREATE TRIGGER _200_primary BEFORE INSERT OR UPDATE ON person_phone
  FOR EACH ROW EXECUTE FUNCTION app_private.tg__person_phone_primary();

CREATE TABLE user_proxy (
  user_id bigint not null references users (u_id) ON UPDATE CASCADE ON DELETE CASCADE,
  person_id bigint not null references person (id) ON UPDATE CASCADE ON DELETE CASCADE,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  PRIMARY KEY (user_id, person_id)
);
create trigger _100_timestamps before insert or update on user_proxy for each row execute procedure app_private.tg__timestamps();
comment on table user_proxy is E'@omit create,update,delete';
GRANT ALL ON TABLE user_proxy TO anonymous;
ALTER TABLE user_proxy ENABLE ROW LEVEL SECURITY;
CREATE INDEX ON user_proxy (user_id);
CREATE INDEX ON user_proxy (person_id);

CREATE TABLE tenant_membership (
  tenant_id bigint not null references tenant (id) ON UPDATE CASCADE ON DELETE CASCADE,
  person_id bigint not null references person (id) ON UPDATE CASCADE ON DELETE CASCADE,
  since timestamptz not null default now(),
  until timestamptz null default null,
  active boolean not null default true,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  PRIMARY KEY (tenant_id, person_id)
);
create trigger _100_timestamps before insert or update on tenant_membership for each row execute procedure app_private.tg__timestamps();
comment on table tenant_membership is E'@omit create,update,delete';
GRANT ALL ON TABLE tenant_membership TO anonymous;
ALTER TABLE tenant_membership ENABLE ROW LEVEL SECURITY;
CREATE INDEX ON tenant_membership (tenant_id);
CREATE INDEX ON tenant_membership (person_id);

CREATE TABLE cohort_membership (
  cohort_id bigint not null references skupiny (s_id) ON UPDATE CASCADE ON DELETE CASCADE,
  person_id bigint not null references person (id) ON UPDATE CASCADE ON DELETE CASCADE,
  since timestamptz not null default now(),
  until timestamptz null default null,
  active boolean not null default true,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  PRIMARY KEY (cohort_id, person_id)
);
create trigger _100_timestamps before insert or update on cohort_membership for each row execute procedure app_private.tg__timestamps();
comment on table cohort_membership is E'@omit create,update,delete';
GRANT ALL ON TABLE cohort_membership TO anonymous;
ALTER TABLE cohort_membership ENABLE ROW LEVEL SECURITY;
CREATE INDEX ON cohort_membership (cohort_id);
CREATE INDEX ON cohort_membership (person_id);

CREATE TABLE tenant_trainer (
  tenant_id bigint not null references tenant (id) ON UPDATE CASCADE ON DELETE CASCADE,
  person_id bigint not null references person (id) ON UPDATE CASCADE ON DELETE CASCADE,
  since timestamptz not null default now(),
  until timestamptz null default null,
  active boolean not null default true,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  PRIMARY KEY (tenant_id, person_id)
);
create trigger _100_timestamps before insert or update on tenant_trainer for each row execute procedure app_private.tg__timestamps();
comment on table tenant_trainer is E'@omit create,update,delete';
GRANT ALL ON TABLE tenant_trainer TO anonymous;
ALTER TABLE tenant_trainer ENABLE ROW LEVEL SECURITY;
CREATE INDEX ON tenant_trainer (tenant_id);
CREATE INDEX ON tenant_trainer (person_id);
CREATE INDEX ON tenant_trainer (active);

CREATE TABLE tenant_administrator (
  tenant_id bigint not null references tenant (id) ON UPDATE CASCADE ON DELETE CASCADE,
  person_id bigint not null references person (id) ON UPDATE CASCADE ON DELETE CASCADE,
  since timestamptz not null default now(),
  until timestamptz null default null,
  active boolean not null default true,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  PRIMARY KEY (tenant_id, person_id)
);
create trigger _100_timestamps before insert or update on tenant_administrator for each row execute procedure app_private.tg__timestamps();
comment on table tenant_administrator is E'@omit create,update,delete';
GRANT ALL ON TABLE tenant_administrator TO anonymous;
ALTER TABLE tenant_administrator ENABLE ROW LEVEL SECURITY;
CREATE INDEX ON tenant_administrator (tenant_id);
CREATE INDEX ON tenant_administrator (person_id);
CREATE INDEX ON tenant_administrator (active);

CREATE TABLE couple (
  id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  man_id bigint not null references person (id) ON UPDATE CASCADE ON DELETE CASCADE,
  woman_id bigint not null references person (id) ON UPDATE CASCADE ON DELETE CASCADE,
  since timestamptz not null default now(),
  until timestamptz null default null,
  active boolean not null default true,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  legacy_pary_id bigint default null
);
create trigger _100_timestamps before insert or update on couple for each row execute procedure app_private.tg__timestamps();
comment on table couple is E'@omit create,update,delete';
GRANT ALL ON TABLE couple TO anonymous;
ALTER TABLE couple ENABLE ROW LEVEL SECURITY;
CREATE INDEX ON couple (man_id);
CREATE INDEX ON couple (woman_id);

create or replace function current_person_ids() returns bigint[] language sql as $$
  select array_agg(person_id) from user_proxy where user_id = current_user_id();
$$;

create or replace function is_current_tenant_member() returns boolean language sql as $$
  select exists (select * from tenant_membership where tenant_id = current_tenant_id() and person_id = any (current_person_ids()));
$$;

--! split: 003-event.sql
drop table if exists event_cohort_registration;
drop table if exists event_instance_trainer;
drop table if exists event_attendance;
drop table if exists event_lesson_demand;
drop table if exists event_registration;
drop table if exists event_target_cohort;
drop table if exists event_instance;
drop table if exists event_trainer;

drop type if exists attendance_type;
create type attendance_type as enum (
  'unknown',
  'attended',
  'excused',
  'not-excused'
);

drop type if exists registration_time;
create type registration_time as enum (
  'pre',
  'regular',
  'post'
);

do $$ begin
  if not exists (SELECT 1 fROM pg_type JOIN pg_enum ON pg_type.oid = pg_enum.enumtypid WHERE typname = 'event_type' and enumlabel = 'holiday') then
    alter type event_type add value 'holiday';
  end if;
end $$;

-- CREATE TABLE public.event (
--     id bigint NOT NULL,
--     tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
--     since date NOT NULL,
--     until date NOT NULL,
--     capacity bigint DEFAULT '0'::bigint NOT NULL,
--     type public.event_type DEFAULT 'camp'::public.event_type NOT NULL,
--     name text NOT NULL,
--     summary text DEFAULT '[]'::jsonb NOT NULL,
--     description text NOT NULL,
--     description_member text DEFAULT ''::text NOT NULL,
--     location_text text NOT NULL,
--     title_image_legacy text,
--     files_legacy text DEFAULT ''::text NOT NULL,
--     updated_at timestamp with time zone,
--     is_locked boolean DEFAULT false NOT NULL,
--     is_visible boolean DEFAULT false NOT NULL,
--     is_public boolean DEFAULT false NOT NULL,
--     enable_notes boolean DEFAULT false NOT NULL
-- );

-- přihlašování OD/DO
-- povolené přihlašování, fáze akce?

drop index if exists event_type_idx;
create index event_type_idx on event (type);

create table event_instance (
  id bigint primary key generated always as identity,
  tenant_id bigint not null references tenant (id) ON UPDATE CASCADE ON DELETE CASCADE DEFAULT public.current_tenant_id(),
  event_id bigint not null references event (id) ON UPDATE CASCADE ON DELETE CASCADE,
  location_id bigint null references location (id) on update cascade on delete set null,
  range tstzrange not null,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);
create trigger _100_timestamps before insert or update on event_instance for each row execute procedure app_private.tg__timestamps();
comment on table event_instance is E'@omit create,update,delete';
GRANT ALL ON TABLE event_instance TO anonymous;
ALTER TABLE event_instance ENABLE ROW LEVEL SECURITY;
create index on event_instance (tenant_id);
create index on event_instance (event_id);
create index on event_instance (location_id);
create index on event_instance using gist (range);

create table event_trainer (
  id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  tenant_id bigint not null references tenant (id) ON UPDATE CASCADE ON DELETE CASCADE DEFAULT public.current_tenant_id(),
  event_id bigint not null references event (id) ON UPDATE CASCADE ON DELETE CASCADE,
  person_id bigint not null references person (id) ON UPDATE CASCADE ON DELETE CASCADE,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);
create trigger _100_timestamps before insert or update on event_trainer for each row execute procedure app_private.tg__timestamps();
comment on table event_trainer is E'@omit create,update,delete';
GRANT ALL ON TABLE event_trainer TO anonymous;
ALTER TABLE event_trainer ENABLE ROW LEVEL SECURITY;
create index on event_trainer (tenant_id);
create index on event_trainer (event_id);
create index on event_trainer (person_id);

create table event_instance_trainer (
  id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  tenant_id bigint not null references tenant (id) ON UPDATE CASCADE ON DELETE CASCADE DEFAULT public.current_tenant_id(),
  instance_id bigint not null references event_instance (id) ON UPDATE CASCADE ON DELETE CASCADE,
  person_id bigint not null references person (id) ON UPDATE CASCADE ON DELETE CASCADE,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);
create trigger _100_timestamps before insert or update on event_instance_trainer for each row execute procedure app_private.tg__timestamps();
comment on table event_instance_trainer is E'@omit create,update,delete';
GRANT ALL ON TABLE event_instance_trainer TO anonymous;
ALTER TABLE event_instance_trainer ENABLE ROW LEVEL SECURITY;
create index on event_instance_trainer (tenant_id);
create index on event_instance_trainer (instance_id);
create index on event_instance_trainer (person_id);

create table event_target_cohort (
  id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  tenant_id bigint not null references tenant (id) ON UPDATE CASCADE ON DELETE CASCADE DEFAULT public.current_tenant_id(),
  event_id bigint not null references event (id) ON UPDATE CASCADE ON DELETE CASCADE,
  cohort_id bigint not null references skupiny (s_id) ON UPDATE CASCADE ON DELETE CASCADE,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);
create trigger _100_timestamps before insert or update on event_target_cohort for each row execute procedure app_private.tg__timestamps();
comment on table event_target_cohort is E'@omit create,update,delete';
GRANT ALL ON TABLE event_target_cohort TO anonymous;
ALTER TABLE event_target_cohort ENABLE ROW LEVEL SECURITY;
create index on event_target_cohort (tenant_id);
create index on event_target_cohort (event_id);
create index on event_target_cohort (cohort_id);

create table event_registration (
  id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  status_time registration_time not null default 'regular',
  tenant_id bigint not null references tenant (id) ON UPDATE CASCADE ON DELETE CASCADE DEFAULT public.current_tenant_id(),
  event_id bigint not null references event (id) ON UPDATE CASCADE ON DELETE CASCADE,
  target_cohort_id bigint null references event_target_cohort (id) on update cascade on delete restrict default null,
  couple_id bigint null references couple (id) ON UPDATE CASCADE ON DELETE CASCADE default null,
  person_id bigint null references person (id) ON UPDATE CASCADE ON DELETE CASCADE default null,
  payment_id bigint null references platby_item (pi_id) ON UPDATE CASCADE ON DELETE CASCADE default null,
  note text null default null,
  is_confirmed boolean default is_current_tenant_member(),
  confirmed_at timestamptz null default case is_current_tenant_member() when true then now() else null end,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  CHECK ((couple_id IS NOT NULL and person_id IS NULL) or (couple_id IS NULL and person_id IS not NULL))
);
create trigger _100_timestamps before insert or update on event_registration for each row execute procedure app_private.tg__timestamps();
comment on table event_registration is E'@omit create,update,delete';
GRANT ALL ON TABLE event_registration TO anonymous;
ALTER TABLE event_registration ENABLE ROW LEVEL SECURITY;
create index on event_registration (tenant_id);
create index on event_registration (event_id);
create index on event_registration (couple_id);
create index on event_registration (person_id);
create index on event_registration (payment_id);
create index on event_registration (target_cohort_id);

create table event_lesson_demand (
  id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  tenant_id bigint not null references tenant (id) ON UPDATE CASCADE ON DELETE CASCADE DEFAULT public.current_tenant_id(),
  trainer_id bigint not null references event_trainer (id) ON UPDATE CASCADE ON DELETE CASCADE,
  registration_id bigint not null references event_registration (id) ON UPDATE CASCADE ON DELETE CASCADE,
  lesson_count int not null check (lesson_count > 0),
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);
create trigger _100_timestamps before insert or update on event_lesson_demand for each row execute procedure app_private.tg__timestamps();
comment on table event_lesson_demand is E'@omit create,update,delete';
GRANT ALL ON TABLE event_lesson_demand TO anonymous;
ALTER TABLE event_lesson_demand ENABLE ROW LEVEL SECURITY;
create index on event_lesson_demand (tenant_id);
create index on event_lesson_demand (trainer_id);
create index on event_lesson_demand (registration_id);

create table event_attendance (
  id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  tenant_id bigint not null references tenant (id) ON UPDATE CASCADE ON DELETE CASCADE DEFAULT public.current_tenant_id(),
  instance_id bigint not null references event_instance (id) ON UPDATE CASCADE ON DELETE CASCADE,
  person_id bigint not null references person (id) ON UPDATE CASCADE ON DELETE CASCADE,
  status attendance_type not null default 'unknown',
  note text null default null,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);
create trigger _100_timestamps before insert or update on event_attendance for each row execute procedure app_private.tg__timestamps();
comment on table event_attendance is E'@omit create,update,delete';
GRANT ALL ON TABLE event_attendance TO anonymous;
ALTER TABLE event_attendance ENABLE ROW LEVEL SECURITY;
create index on event_attendance (tenant_id);
create index on event_attendance (instance_id);
create index on event_attendance (person_id);

--! split: 004-transactions.sql
alter table platby_item drop column if exists status;
drop type if exists payment_status;
create type payment_status as enum (
  'tentative',
  'unpaid',
  'paid'
);
alter table platby_item add column if not exists status payment_status not null default 'paid';


-- platby_item: add variable symbol
-- platby_item: transaction id
-- drop payment_group
-- make payment_category optional???

--! split: 100-migration.sql
CREATE or replace FUNCTION app_private.regenerate_table_person() RETURNS void LANGUAGE plpgsql STRICT SECURITY DEFINER AS $$
declare
  row users;
  address_id bigint;
  person_id bigint;
  tenant_id bigint = 1::bigint;
begin
  delete from person;
  FOR row IN SELECT * FROM users where not exists (select 1 from person where legacy_user_id=u_id) LOOP
    insert into person (legacy_user_id, first_name, last_name, gender, birth_date, tax_identification_number, nationality)
    values (
      row.u_id,
      row.u_jmeno,
      row.u_prijmeni,
      (case row.u_pohlavi when 'm' then 'man' else 'woman' end)::gender_type,
      row.u_narozeni,
      row.u_rodne_cislo,
      row.u_nationality
    ) returning id into person_id;

    insert into address (street, conscription_number, orientation_number, district, city, postal_code)
    values (row.u_street, row.u_conscription_number, row.u_orientation_number, row.u_district, row.u_city, row.u_postal_code)
    returning id into address_id;

    insert into person_address (person_id, address_id) values (person_id, address_id);
    insert into person_email (person_id, email) values (person_id, row.u_email);
    insert into person_phone (person_id, phone) values (person_id, row.u_telefon);
    insert into user_proxy (user_id, person_id) values (row.u_id, person_id);
    insert into tenant_membership (tenant_id, person_id, since, until, active)
    values (
      tenant_id,
      person_id,
      COALESCE(row.u_member_since, row.u_created_at),
      case row.u_ban when true then row.u_timestamp else null end,
      not row.u_ban
    );
    insert into cohort_membership (cohort_id, person_id, since, until, active)
    values (
      row.u_skupina,
      person_id,
      COALESCE(row.u_member_since, row.u_created_at),
      case row.u_ban when true then row.u_timestamp else null end,
      not row.u_ban
    );

    if exists (select * from rozpis where r_trener = row.u_id) then
      insert into tenant_trainer (tenant_id, person_id, since, until, active)
      values (tenant_id, person_id, COALESCE(row.u_member_since, row.u_created_at), NULL, not row.u_ban);
    end if;

    if row.u_group in (1, 9) then
      insert into tenant_administrator (tenant_id, person_id, since, until, active)
      values (tenant_id, person_id, COALESCE(row.u_member_since, row.u_created_at), NULL, not row.u_ban);
    end if;
  end loop;
end;
$$;
select verify_function('app_private.regenerate_table_person');

CREATE or replace FUNCTION app_private.regenerate_table_couple() RETURNS void LANGUAGE plpgsql STRICT SECURITY DEFINER AS $$
declare
  row pary;
  man person;
  woman person;
begin
  delete from couple;
  for row in select * from pary LOOP
    if row.p_id_partnerka is null then continue; end if;
    select * into man from person where legacy_user_id = row.p_id_partner;
    select * into woman from person where legacy_user_id = row.p_id_partnerka;
    insert into couple (legacy_pary_id, man_id, woman_id, since, until, active)
    values (row.p_id, man.id, woman.id, row.p_timestamp_add, row.p_timestamp_archive, not row.p_archiv);
  end loop;
end;
$$;
select verify_function('app_private.regenerate_table_couple');

CREATE or replace FUNCTION app_private.regenerate_event_reservation() RETURNS void LANGUAGE plpgsql STRICT SECURITY DEFINER AS $$
declare
  schedule nabidka;
  lesson nabidka_item;
  item event;
  item_instance event_instance;
  par pary;
  trainer event_trainer;
  reg event_registration;
begin
  delete from event where "type" = 'reservation';
  for schedule in select * from nabidka loop
    insert into event (name, description, location_text, type, since, until, capacity, is_locked, is_visible)
    values ('', '', '', 'reservation', schedule.n_od + time '00:00', schedule.n_do + time '23:59:59.999999', schedule.n_pocet_hod, schedule.n_lock, schedule.n_visible)
    returning * into item;

    insert into event_instance (event_id, range)
    values (item.id, tstzrange(schedule.n_od + time '00:00', schedule.n_do + time '23:59:59.999999', '[]'))
    returning * into item_instance;

    insert into event_trainer (event_id, person_id)
    select item.id, person.id from person where legacy_user_id = schedule.n_trener
    returning * into trainer;
    insert into event_instance_trainer (instance_id, person_id)
    select item_instance.id, person.id from person where legacy_user_id = schedule.n_trener;

    for lesson in select * from nabidka_item where ni_id_rodic = schedule.n_id loop
      select * into par from pary where p_id = lesson.ni_partner;
      if par.p_id_partnerka is null then
        insert into event_registration (event_id, person_id, is_confirmed)
        select item.id, person.id, true from person where legacy_user_id = par.p_id_partner
        returning * into reg;
        insert into event_lesson_demand (registration_id, trainer_id, lesson_count)
        values (reg.id, trainer.id, lesson.ni_pocet_hod);
      else
        insert into event_registration (event_id, couple_id, is_confirmed)
        select item.id, couple.id, true from couple where legacy_pary_id = lesson.ni_partner
        returning * into reg;
        insert into event_lesson_demand (registration_id, trainer_id, lesson_count)
        values (reg.id, trainer.id, lesson.ni_pocet_hod);
      end if;
    end loop;
  end loop;
end;
$$;
select verify_function('app_private.regenerate_event_reservation');

CREATE or replace FUNCTION app_private.regenerate_event_lesson() RETURNS void LANGUAGE plpgsql STRICT SECURITY DEFINER AS $$
declare
  schedule rozpis;
  lesson rozpis_item;
  item event;
  item_instance event_instance;
  par pary;
begin
  delete from event where "type" = 'lesson';
  for lesson in select * from rozpis_item loop
    select * into schedule from rozpis where r_id = lesson.ri_id_rodic;

    insert into event (name, description, type, since, until, location_text, is_locked, is_visible)
    values ('', '', 'lesson', schedule.r_datum + lesson.ri_od, schedule.r_datum + lesson.ri_do, schedule.r_kde, schedule.r_lock, schedule.r_visible)
    returning * into item;
    insert into event_instance (event_id, range)
    values (item.id, tstzrange(schedule.r_datum + lesson.ri_od, schedule.r_datum + lesson.ri_do, '[]'))
    returning * into item_instance;

    insert into event_trainer (event_id, person_id)
    select item.id, person.id from person where legacy_user_id = schedule.r_trener;
    insert into event_instance_trainer (instance_id, person_id)
    select item_instance.id, person.id from person where legacy_user_id = schedule.r_trener;

    if lesson.ri_partner is null then
      select * into par from pary where p_id = lesson.ri_partner;
      if par.p_id_partnerka is not null then
        insert into event_registration (event_id, person_id, is_confirmed)
        select item.id, person.id, true from person where legacy_user_id = par.p_id_partner;
      else
        insert into event_registration (event_id, couple_id, is_confirmed)
        select item.id, couple.id, true from couple where legacy_pary_id = lesson.ri_partner;
      end if;
    end if;
  end loop;
end;
$$;
select verify_function('app_private.regenerate_event_lesson');
