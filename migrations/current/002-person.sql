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
