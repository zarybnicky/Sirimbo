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
ALTER TABLE pary ADD CONSTRAINT pary_p_id_partnerka_fkey FOREIGN KEY (p_id_partnerka) REFERENCES users (u_id);

comment on table pary is E'@omit create, update, delete';

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

DROP TABLE if exists user_proxy;
drop table if exists couple;
drop table if exists cohort_membership;
DROP TABLE if exists tenant_administrator;
DROP TABLE if exists tenant_trainer;
DROP TABLE if exists tenant_membership;
DROP TABLE if exists tenant_person;
DROP TABLE if exists person_address;
DROP TABLE if exists person_email;
DROP TABLE if exists person_phone;
DROP TABLE if exists person;
DROP TABLE if exists address;

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
  identity_number text,
  nationality text not null,
  csts_id number default null,
  wdsf_id number default null,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
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
  person_id bigint not null references person (id),
  address_id bigint not null references address (id),
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
  person_id bigint not null references person (id),
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
  user_id bigint not null references users (u_id),
  person_id bigint not null references person (id),
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
  tenant_id bigint not null references tenant (id),
  person_id bigint not null references person (id),
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
  cohort_id bigint not null references skupiny (s_id),
  person_id bigint not null references person (id),
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
  tenant_id bigint not null references tenant (id),
  person_id bigint not null references person (id),
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

CREATE TABLE tenant_administrator (
  tenant_id bigint not null references tenant (id),
  person_id bigint not null references person (id),
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

CREATE TABLE couple (
  id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  legacy_pary_id bigint default null,
  man_id bigint not null references person (id),
  woman_id bigint not null references person (id),
  since timestamptz not null default now(),
  until timestamptz null default null,
  active boolean not null default true,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);
create trigger _100_timestamps before insert or update on couple for each row execute procedure app_private.tg__timestamps();
comment on table couple is E'@omit create,update,delete';
GRANT ALL ON TABLE couple TO anonymous;
ALTER TABLE couple ENABLE ROW LEVEL SECURITY;
CREATE INDEX ON couple (man_id);
CREATE INDEX ON couple (woman_id);

-- person -> add ČSTS + WDSF no.s, unique! or NULL
-- add sync fns, or just delete all old uses?

-- rozpis + nabidka -> event + type, later just event
-- separate event page, slowly fill it in

-- disable registration, change reg form to register + membership application

do $$
declare
  row users;
  gender gender_type;
  address_id bigint;
  person_id bigint;
  tenant_id bigint = 1;
begin
  FOR row IN SELECT * FROM users LOOP
    select case row.u_pohlavi when 'm' then 'man' else 'woman' end into gender;
    insert into person (first_name, last_name, gender, birth_date, identity_number, nationality)
    values (row.u_jmeno, row.u_prijmeni, gender, row.u_narozeni, row.u_rodne_cislo, row.u_nationality)
    returning id into person_id;

    insert into address (street, conscription_number, orientation_number, district, city, postal_code)
    values (row.u_street, row.u_conscription_number, row.u_orientation_number, row.u_district, row.u_city, row.u_postal_code)
    returning id into address_id;

    insert into person_address (person_id, address_id) values (person_id, address_id);
    insert into person_email (person_id, email) values (person_id, row.u_email);
    insert into person_phone (person_id, phone) values (person_id, row.u_telefon);
    insert into user_proxy (user_id, person_id) values (row.u_id, person_id);
    insert into tenant_membership (tenant_id, person_id, since, until, active)
    values (tenant_id, person_id, COALESCE(row.u_member_since, row.u_created_at), case row.u_ban when true then row.u_timestamp else null end, not row.u_ban);
    insert into cohort_membership (cohort_id, person_id, since, until, active)
    values (row.u_skupina, person_id, COALESCE(row.u_member_since, row.u_created_at), case row.u_ban when true then row.u_timestamp else null end, not row.u_ban);

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

do $$
declare
  row pary;
  man person;
  woman person;
begin
  for row in select * from pary LOOP
    if row.p_id_partnerka is null then continue; end if;
    select person.* into man from person inner join user_proxy on person_id=person.id inner join users on user_id=u_id where u_id=row.p_id_partner;
    select person.* into woman from person inner join user_proxy on person_id=person.id inner join users on user_id=u_id where u_id=row.p_id_partnerka;
    insert into couple (legacy_pary_id, man_id, woman_id, since, until, active)
    values (row.p_id, man.id, woman.id, row.p_timestamp_add, row.p_timestamp_archive, not row.p_archiv);
  end loop;
end;
$$;
