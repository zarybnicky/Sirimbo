alter table if exists public.pary_navrh set schema app_private;
alter table if exists public.parameters set schema app_private;

COMMENT ON TABLE public.attendee_external IS E'@omit';
COMMENT ON TABLE public.attendee_user IS E'@omit';
COMMENT ON TABLE public.rozpis IS E'@omit';
COMMENT ON TABLE public.rozpis_item IS E'@omit';
COMMENT ON TABLE public.nabidka IS E'@omit';
COMMENT ON TABLE public.nabidka_item IS E'@omit';
comment on table couple is E'@omit update,delete';
comment on table cohort_membership is E'@omit create,update,delete
@simpleCollections only';
comment on table tenant_membership is E'@omit create,update,delete
@simpleCollections only';
comment on table tenant_administrator is E'@omit create,update,delete
@simpleCollections only';
comment on table tenant_trainer is E'@omit create,update,delete
@simpleCollections only';
comment on table session is E'@omit';
comment on table room_attachment is E'@omit update,order,filter';
comment on table tenant_attachment is E'@omit update,order,filter';
comment on table permissions is E'@omit insert,update,delete,order,filter\n@simpleCollections only';
comment on function verify_function is E'@omit';
comment on table address is E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.person_address IS E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.person_phone IS E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.person_email IS E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.event_attendance IS E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.event_instance IS E'@omit create,update,delete
@simpleCollections both';
COMMENT ON TABLE public.event_instance_trainer IS E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.event_lesson_demand IS E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.event_target_cohort IS E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.event_registration IS E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.event_trainer IS E'@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.platby_raw IS E'@omit create,update,delete';
COMMENT ON TABLE public.platby_group_skupina IS E'@omit create,update,delete';
COMMENT ON TABLE public.tenant IS E'@omit create,delete
@simpleCollections only';
COMMENT ON TABLE public.user_proxy IS E'@omit create,update,delete
@simpleCollections only';

create index if not exists couple_active_idx on couple (active);
create index if not exists tenant_membership_active_idx on tenant_membership (active);
create index if not exists cohort_membership_active_idx on cohort_membership (active);
create index if not exists tenant_administrator_active_idx on tenant_administrator (active);
create index if not exists tenant_trainer_active_idx on tenant_trainer (active);

alter table cohort_group alter column description set default '';
alter table event alter column summary set default '';
alter table skupiny alter column internal_info set default '';

do $$ begin
  if not exists (select 1 from information_schema.columns where table_name = 'tenant_administrator' and column_name = 'id') then
    ALTER TABLE tenant_administrator DROP CONSTRAINT tenant_administrator_pkey;
    ALTER TABLE tenant_administrator ADD COLUMN id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY;
  end if;
end $$;
do $$ begin
  if not exists (select 1 from information_schema.columns where table_name = 'tenant_trainer' and column_name = 'id') then
    ALTER TABLE tenant_trainer DROP CONSTRAINT tenant_trainer_pkey;
    ALTER TABLE tenant_trainer ADD COLUMN id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY;
  end if;
end $$;
do $$ begin
  if not exists (select 1 from information_schema.columns where table_name = 'tenant_membership' and column_name = 'id') then
    ALTER TABLE tenant_membership DROP CONSTRAINT tenant_membership_pkey;
    ALTER TABLE tenant_membership ADD COLUMN id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY;
  end if;
end $$;
do $$ begin
  if not exists (select 1 from information_schema.columns where table_name = 'cohort_membership' and column_name = 'id') then
    ALTER TABLE cohort_membership DROP CONSTRAINT cohort_membership_pkey;
    ALTER TABLE cohort_membership ADD COLUMN id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY;
  end if;
end $$;
do $$ begin
  if not exists (select 1 from information_schema.columns where table_name = 'user_proxy' and column_name = 'id') then
    ALTER TABLE user_proxy DROP CONSTRAINT user_proxy_pkey;
    ALTER TABLE user_proxy ADD COLUMN id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY;
  end if;
end $$;

drop table if exists tenant_person;
drop function if exists fix_unpaired_couples;
drop function if exists prospect_form_dancer;
drop function if exists legacy_duplicate_rozpis;
drop function if exists legacy_duplicate_nabidka;
drop function if exists current_permissions;
drop function if exists insert_revision;
drop function if exists event_free_slots;
drop function if exists nabidka_free_lessons;
drop function if exists nabidka_my_lessons;
drop function if exists reservation_set_desired_lessons;
drop function if exists event_remaining_spots;
drop function if exists book_lesson;
drop function if exists cancel_lesson;
drop function if exists cancel_participation;
drop function if exists create_participation;
drop function if exists create_participation_external;
drop function if exists schedules_for_range;
drop function if exists reservations_for_range;
drop function if exists active_couples;
drop function if exists create_couple;

create or replace function regenerate_event_registration_from_attendees() returns void language plpgsql security definer as $$
begin
  insert into event_registration (tenant_id, event_id, person_id, note, is_confirmed)
  select
    attendee_user.tenant_id,
    attendee_user.event_id,
    (select id from person where legacy_user_id = attendee_user.user_id),
    case attendee_user.notes when '' then null else attendee_user.notes end,
    true
  from attendee_user;
end;
$$;
select verify_function('regenerate_event_registration_from_attendees');
