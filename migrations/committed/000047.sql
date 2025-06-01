--! Previous: sha1:5746c7232755638f52c833a6008964ed30fe2f5b
--! Hash: sha1:652bf2ab184977bf52de5c5eff53f759eaa87770

--! split: 1-current.sql
COMMENT ON TABLE public.transaction IS '@omit create,update';

do $$ begin
  if exists (select 1 from information_schema.columns where table_name = 'room' and column_name = 'location') then
    ALTER TABLE room rename COLUMN location to location_id;
  end if;
end $$;

drop table if exists event_external_registration;
create table if not exists event_external_registration (
    id bigint primary key generated always as identity,
    tenant_id bigint not null references tenant (id) ON UPDATE CASCADE ON DELETE CASCADE DEFAULT current_tenant_id(),
    event_id bigint not null references event (id) ON UPDATE CASCADE ON DELETE CASCADE,

    first_name text NOT NULL,
    last_name text NOT NULL,
    prefix_title text DEFAULT '' NOT NULL,
    suffix_title text DEFAULT '' NOT NULL,
    nationality text NOT NULL,
    birth_date date,
    tax_identification_number text,
    email citext not null,
    phone text not null,

    note text,
    created_by bigint null references users (id) default current_user_id(),
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.event_external_registration IS '@omit update
@simpleCollections only';

GRANT ALL ON TABLE event_external_registration TO anonymous;
REVOKE INSERT ON TABLE event_external_registration FROM anonymous;
GRANT INSERT (event_id, note) ON TABLE event_external_registration TO anonymous;

ALTER TABLE public.event_external_registration ENABLE ROW LEVEL SECURITY;

CREATE POLICY admin_all ON event_external_registration TO administrator USING (true);
CREATE POLICY trainer_same_tenant ON event_external_registration TO trainer USING (
  app_private.can_trainer_edit_event(event_id)
) WITH CHECK (true);
CREATE POLICY admin_my ON event_external_registration TO member USING (
  (SELECT event_is_registration_open(event.*) FROM event WHERE event_id = event.id)
  AND (created_by = current_user_id())
);
CREATE POLICY view_visible_event ON public.event_external_registration FOR SELECT TO member USING (
  EXISTS (SELECT 1 FROM event WHERE event_id = event.id)
);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON event_external_registration
  FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();


CREATE or replace FUNCTION event_trainer_lessons_remaining(e event_trainer) RETURNS integer AS $$
  select e.lessons_offered - (
    select coalesce(sum(lesson_count), 0)
    from event_lesson_demand where trainer_id = e.id
  );
$$ LANGUAGE sql STABLE;

CREATE or replace FUNCTION event_remaining_person_spots(e event) RETURNS integer AS $$
  select e.capacity - (
    select coalesce(sum(case when couple_id is not null then 2 else 1 end), 0)
    from event_registration where event_id = e.id
  ) - (
    select coalesce(count(id), 0)
    from event_external_registration where event_id = e.id
  );
$$ LANGUAGE sql STABLE security definer;

CREATE or replace FUNCTION event_remaining_lessons(e event) RETURNS integer AS $$
  select (
    select coalesce(sum(lessons_offered), 0) from event_trainer where event_id = e.id
  ) - (
    select coalesce(sum(lesson_count), 0) from event_registration join event_lesson_demand on registration_id = event_registration.id where event_id = e.id
  );
$$ LANGUAGE sql STABLE security definer;

GRANT ALL ON FUNCTION event_trainer_lessons_remaining(e event_trainer) TO anonymous;
GRANT ALL ON FUNCTION event_remaining_person_spots(e event) TO anonymous;
GRANT ALL ON FUNCTION event_remaining_lessons(e event) TO anonymous;
