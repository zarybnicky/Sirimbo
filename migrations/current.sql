drop table if exists lesson_trainer;
drop table if exists lesson;
drop table if exists attendee_person;
drop table if exists attendee_couple;
drop table if exists attendee_list;
drop table if exists couple;

create table couple (
  id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  tenant_id bigint not null references tenant (id) DEFAULT public.current_tenant_id(),
  since timestamptz not null,
  until timestamptz not null,
  leader_id bigint not null references person (id),
  follower_id bigint not null references person (id),
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);
create trigger _100_timestamps before insert or update on couple
  for each row execute procedure app_private.tg__timestamps();
comment on table couple is E'@omit create,update,delete';
create index on couple (tenant_id);
create index on couple (leader_id);
create index on couple (follower_id);

create table attendee_list (
  id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  tenant_id bigint not null references tenant (id) DEFAULT public.current_tenant_id(),
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);
create trigger _100_timestamps before insert or update on attendee_list
  for each row execute procedure app_private.tg__timestamps();
comment on table attendee_list is E'@omit create,update,delete';
create index on attendee_list (tenant_id);

create table attendee_couple (
  id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  tenant_id bigint not null references tenant (id) DEFAULT public.current_tenant_id(),
  list_id bigint not null references attendee_list (id),
  couple_id bigint not null references couple (id),
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);
create trigger _100_timestamps before insert or update on attendee_couple
  for each row execute procedure app_private.tg__timestamps();
comment on table attendee_couple is E'@omit create,update,delete';
create index on attendee_couple (tenant_id);
create index on attendee_couple (list_id);
create index on attendee_couple (couple_id);

create table attendee_person (
  id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  tenant_id bigint not null references tenant (id) DEFAULT public.current_tenant_id(),
  list_id bigint not null references attendee_list (id),
  person_id bigint not null references person (id),
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);
create trigger _100_timestamps before insert or update on attendee_person
  for each row execute procedure app_private.tg__timestamps();
comment on table attendee_person is E'@omit create,update,delete';
create index on attendee_person (tenant_id);
create index on attendee_person (list_id);
create index on attendee_person (person_id);

create table lesson (
  id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  tenant_id bigint not null references tenant (id) DEFAULT public.current_tenant_id(),
  since timestamptz not null,
  until timestamptz not null,
  location_id bigint null references location (id),
  location_text text,
  list_id bigint not null references attendee_list (id),
  is_locked boolean not null default false,
  is_visible boolean not null default true,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);
create trigger _100_timestamps before insert or update on lesson
  for each row execute procedure app_private.tg__timestamps();
comment on table lesson is E'@omit create,update,delete';
create index on lesson (tenant_id);
create index on lesson (list_id);
create index on lesson (location_id);

create table lesson_trainer (
  id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  tenant_id bigint not null references tenant (id) DEFAULT public.current_tenant_id(),
  lesson_id bigint not null references lesson (id),
  trainer_id bigint not null references users (u_id)
);
comment on table lesson_trainer is E'@omit create,update,delete';
create index on lesson_trainer (tenant_id);
create index on lesson_trainer (lesson_id);
create index on lesson_trainer (trainer_id);


do $$
begin
  if not exists (select * from tenant where id = 2) then
    insert into tenant (id, name, member_info) values (2, 'DSP Kometa Brno', '[]'::json);
  end if;

  if not exists (select * from location where name = 'TS Kometa') then
    insert into location (id, name, description) values (3, 'TS Kometa', '[]'::json);
  end if;

  if not exists (select * from tenant_location where tenant_id = 2) then
    insert into tenant_location (tenant_id, location_id) values (2, 3);
  end if;
end $$;
