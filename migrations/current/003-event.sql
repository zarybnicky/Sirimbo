
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
