--! Previous: sha1:82aad5d0439232ad52ea3b6646e76dbfc6765c76
--! Hash: sha1:1748ccce91866980da17ae8e6c992182ea57aac2

drop EXTENSION if exists plpgsql_check;
create extension plpgsql_check;

do $$
begin
  if not exists (select * from pg_type where typcategory='E' and typname = 'event_type') then
    create type event_type as enum (
      'camp',
      'lesson',
      'reservation'
    );
  end if;
end
$$;

alter table event add column if not exists "type" event_type not null default 'camp';

create or replace VIEW public.akce AS
 SELECT event.id AS a_id,
    event.name AS a_jmeno,
    event.location_text AS a_kde,
    event.description AS a_info,
    event.since AS a_od,
    event.until AS a_do,
    event.capacity AS a_kapacita,
    event.files_legacy AS a_dokumenty,
    event.updated_at AS a_timestamp,
    event.is_locked AS a_lock,
    event.is_visible AS a_visible,
    event.summary,
    event.is_public,
    event.enable_notes
   FROM public.event
   where type='camp';

alter table upozorneni_skupiny drop column if exists ups_popis;


CREATE or replace FUNCTION public.cancel_participation(event_id bigint) RETURNS void LANGUAGE plpgsql STRICT SECURITY DEFINER AS $$
#variable_conflict use_variable
declare
  event event;
begin
  select * into event from event where id=event_id;
  if event is null then
    raise exception 'ITEM_NOT_FOUND' using errcode = '28000';
  end if;
  if event.is_locked then
    raise exception 'ITEM_LOCKED' using errcode = '42501';
  end if;

  delete from attendee_user where attendee_user.event_id=event_id and user_id=current_user_id();
end;
$$;
GRANT ALL ON FUNCTION public.cancel_participation(event_id bigint) TO member;
select verify_function('cancel_participation');

drop FUNCTION if exists public.create_participation(event_id bigint, year_of_birth integer, my_notes text);
CREATE or replace FUNCTION public.create_participation(event_id bigint, my_notes text) RETURNS void LANGUAGE plpgsql STRICT SECURITY DEFINER AS $$
#variable_conflict use_column
declare
  event event;
begin
  select * into event from event where id=event_id;
  if event is null then
    raise exception 'ITEM_NOT_FOUND' using errcode = '28000';
  end if;
  if event.is_locked then
    raise exception 'ITEM_LOCKED' using errcode = '42501';
  end if;

  INSERT INTO attendee_user (event_id, user_id, notes)
  values (event_id, current_user_id(), my_notes)
  ON CONFLICT (user_id, event_id) DO UPDATE SET notes = my_notes;
end;
$$;
GRANT ALL ON FUNCTION public.create_participation(event_id bigint, my_notes text) TO member;
select verify_function('create_participation');

drop function if exists akce_my_notes(public.event);
CREATE or replace FUNCTION public.event_my_notes(a public.event) RETURNS text LANGUAGE sql STABLE AS $$
  select notes from attendee_user where event_id=a.id and user_id=current_user_id();
$$;
GRANT ALL ON FUNCTION public.event_my_notes(a public.event) TO anonymous;

drop FUNCTION if exists akce_has_capacity(a public.event);
CREATE or replace FUNCTION public.event_has_capacity(a public.event) RETURNS boolean LANGUAGE sql STABLE AS $$
  select count(*) < a.capacity from attendee_user where event_id = a.id;
$$;
GRANT ALL ON FUNCTION public.event_has_capacity(a public.event) TO anonymous;

drop FUNCTION if exists public.akce_signed_up(public.event);
CREATE or replace FUNCTION public.event_signed_up(a public.event) RETURNS boolean LANGUAGE sql STABLE AS $$
  select exists (select id from attendee_user where event_id=a.id and user_id=current_user_id());
$$;
GRANT ALL ON FUNCTION public.event_signed_up(a public.event) TO anonymous;

drop FUNCTION if exists public.akce_free_slots(public.event);
CREATE or replace FUNCTION public.event_free_slots(a public.event) RETURNS integer LANGUAGE sql STABLE AS $$
  select a.capacity - (select count(*) from attendee_user where event_id = a.id);
$$;
GRANT ALL ON FUNCTION public.event_free_slots(a public.event) TO anonymous;

drop view if exists akce_item;
drop view if exists akce;
alter table attendee_user drop column if exists birth_year;
