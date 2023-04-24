do $$ begin
  if not exists (select 1 from information_schema.tables
    where table_schema = 'public' and table_name = 'event'
  ) then
    ALTER TABLE akce RENAME TO event;
    ALTER TABLE event RENAME COLUMN a_id TO id;
    ALTER TABLE event RENAME COLUMN a_jmeno TO name;
    ALTER TABLE event RENAME COLUMN a_kde TO location_text;
    ALTER TABLE event RENAME COLUMN a_info TO description;
    ALTER TABLE event RENAME COLUMN a_od TO since;
    ALTER TABLE event RENAME COLUMN a_do TO until;
    ALTER TABLE event RENAME COLUMN a_kapacita TO capacity;
    ALTER TABLE event RENAME COLUMN a_dokumenty TO files_legacy;
    ALTER TABLE event RENAME COLUMN a_timestamp TO updated_at;
    ALTER TABLE event RENAME COLUMN a_lock TO is_locked;
    ALTER TABLE event RENAME COLUMN a_visible TO is_visible;
  end if;
end $$;

DROP VIEW IF EXISTS akce CASCADE;
CREATE VIEW akce AS
  SELECT
    id            as a_id,
    name          as a_jmeno,
    location_text as a_kde,
    description   as a_info,
    since         as a_od,
    until         as a_do,
    capacity      as a_kapacita,
    files_legacy  as a_dokumenty,
    updated_at    as a_timestamp,
    is_locked     as a_lock,
    is_visible    as a_visible,

    summary,
    is_public,
    enable_notes
  FROM event;

do $$ begin
  if not exists (select 1 from information_schema.tables
    where table_schema = 'public' and table_name = 'attendee_user'
  ) then
    ALTER TABLE akce_item RENAME TO attendee_user;
    ALTER TABLE attendee_user RENAME COLUMN ai_id to id;
    ALTER TABLE attendee_user RENAME COLUMN ai_id_rodic to event_id;
    ALTER TABLE attendee_user RENAME COLUMN ai_user to user_id;
    ALTER TABLE attendee_user RENAME COLUMN ai_rok_narozeni to birth_year;
  end if;
end $$;

DROP VIEW IF EXISTS akce_item CASCADE;
CREATE VIEW akce_item as
  SELECT
    id       AS ai_id,
    event_id AS ai_id_rodic,
    user_id  AS ai_user,
    birth_year AS ai_rok_narozeni,

    notes
  FROM attendee_user;

DROP TABLE IF EXISTS attendee_external CASCADE;
CREATE TABLE attendee_external (
    id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    event_id bigint REFERENCES event,
    first_name text not null,
    last_name text not null,
    email text not null,
    phone text not null,

    notes text not null,
    birth_number text default null,
    guardian_name text not null default '',

    is_confirmed boolean not null default false,
    created_at timestamptz not null default now(),
    updated_at timestamptz not null default now()
);
alter table attendee_external enable row level security;
create policy manage_all on attendee_external to administrator using (true) with check (true);
create policy insert_all on attendee_external for insert to anonymous with check (is_confirmed = false);
create policy select_member on attendee_external for select to member using (true);
grant all on attendee_external to anonymous;

create or replace function event_remaining_spots (a event) returns int as $$
  select a.capacity - (select count(*) from attendee_user where event_id = a.id) - (select count(*) from attendee_external where event_id = a.id);
$$ language sql stable;
grant execute on function event_remaining_spots to anonymous;
