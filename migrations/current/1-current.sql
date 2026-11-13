
--!include functions/resend_person_invitation.sql
--!include policies/event_registration.sql

do $$
begin
  if not exists (
    select 1 from information_schema.columns
    where table_schema = 'public' and table_name = 'person_invitation' and column_name = 'last_sent_at'
  ) then
    alter table person_invitation add column last_sent_at timestamptz;
    update person_invitation set last_sent_at = created_at;
  end if;
end;
$$;

do $$
begin
  if to_regtype('public.event_search_condition') is null then
    create type event_search_condition as (
      types event_type[],
      trainer_ids bigint[],
      location_ids bigint[],
      parent_id bigint,
      participant_ids bigint[],
      cohort_id bigint,
      only_mine boolean,
      relevant_only boolean,
      can_register boolean,
      with_competitions boolean
    );
  end if;
end;
$$;

-- Preserve existing saved conditions while extending their composite type.
do $$
begin
  if not exists (
    select 1 from pg_attribute
    where attrelid = (select typrelid from pg_type where oid = 'event_search_condition'::regtype)
      and attname = 'couple_id' and not attisdropped
  ) then
    alter type event_search_condition add attribute couple_id bigint cascade;
  end if;
end;
$$;

create table if not exists event_saved_search (
  id bigint generated always as identity primary key,
  tenant_id bigint not null default current_tenant_id() references tenant(id) on delete cascade,
  user_id bigint not null default current_user_id() references users(id) on delete cascade,
  name text not null,
  condition event_search_condition not null,
  feed_key uuid unique default gen_random_uuid(),
  cross_tenant boolean not null default false
);

comment on table event_saved_search is '@behavior +list -connection';
comment on constraint event_saved_search_feed_key_key on event_saved_search is
  '@behavior -query:resource:single -constraint:resource:update -constraint:resource:delete';
comment on column event_saved_search.user_id is '@behavior -attribute:insert -attribute:update';
comment on column event_saved_search.tenant_id is '@behavior -attribute:insert -attribute:update';
comment on column event_saved_search.condition is E'@behavior -attribute:filterBy -attribute:orderBy';
comment on column event_saved_search.feed_key is E'@behavior -attribute:filterBy -attribute:orderBy';

alter table event_saved_search enable row level security;

select app_private.drop_policies('public.event_saved_search');
create policy owner on event_saved_search
  using (user_id = (select current_user_id()))
  with check (user_id = (select current_user_id()));
revoke all on event_saved_search from public, anonymous;
grant select, delete on event_saved_search to anonymous;
grant insert (name, condition, feed_key, cross_tenant),
  update (name, condition, feed_key, cross_tenant) on event_saved_search to anonymous;

create index if not exists event_saved_search_user_id_idx on event_saved_search (user_id, tenant_id);

--!include functions/event_facets.sql
--!include functions/save_payment_group.sql
--!include functions/resolve_payment_with_credit.sql
--!include functions/create_latest_lesson_payments.sql

comment on table cohort_subscription is '@behavior +list -connection -insert -update -delete -query:resource:connection -query:resource:single';

alter table couple add column if not exists name text;
alter table couple add column if not exists short_name text;

--!include functions/tg_couple__names.sql

update couple set name = name where name is null or short_name is null;
alter table couple alter column name set not null;
alter table couple alter column short_name set not null;

comment on column couple.name is '@behavior -attribute:insert -attribute:update';
comment on column couple.short_name is '@behavior -attribute:insert -attribute:update';

alter table person add column if not exists search_vector tsvector
  generated always as (to_tsvector('pg_catalog.simple', app_private.normalize_name(
    immutable_concat_ws(' ', first_name, last_name)))) stored;
alter table cohort add column if not exists search_vector tsvector
  generated always as (to_tsvector('pg_catalog.simple', app_private.normalize_name(name))) stored;
alter table location add column if not exists search_vector tsvector
  generated always as (to_tsvector('pg_catalog.simple', app_private.normalize_name(
    immutable_concat_ws(' ', name, long_name)))) stored;
alter table event add column if not exists search_vector tsvector
  generated always as (to_tsvector('pg_catalog.simple', app_private.normalize_name(coalesce(name, '')))) stored;
alter table couple add column if not exists search_vector tsvector
  generated always as (to_tsvector('pg_catalog.simple', app_private.normalize_name(name))) stored;

comment on column person.search_vector is '@behavior -*';
comment on column cohort.search_vector is '@behavior -*';
comment on column location.search_vector is '@behavior -*';
comment on column event.search_vector is '@behavior -*';
comment on column couple.search_vector is '@behavior -*';

create index if not exists couple_search_vector_idx on couple using gin (search_vector);
create index if not exists person_search_vector_idx on person using gin (search_vector);
create index if not exists cohort_search_vector_idx on cohort using gin (search_vector);
create index if not exists location_search_vector_idx on location using gin (search_vector);
create index if not exists event_search_vector_idx on event using gin (search_vector);

drop function if exists global_search(text, integer);
drop type if exists global_search_result;
drop type if exists global_search_kind;

create type global_search_result as (
  person person,
  couple couple,
  cohort cohort,
  location location,
  event event,
  score real,
  position integer
);

comment on column global_search_result.score is '@notNull';
comment on column global_search_result.position is '@notNull';

--!include functions/current_person_ids.sql

create index if not exists event_visibility_idx on event (tenant_id, id)
  include (is_public, is_visible, manager_person_ids);

select app_private.drop_policies('public.event');
select app_private.drop_policies('public.event_trainer');
select app_private.drop_policies('public.event_target_cohort');
drop function if exists app_private.accessible_event_ids();

--!include functions/accessible_event_ids.sql
--!include functions/global_search.sql
--!include policies/event.sql
--!include policies/event_trainer.sql
--!include policies/event_target_cohort.sql
--!include functions/cohort_has_accessible_event.sql
--!include functions/visible_file_ids.sql

--!include views/competition_participation.sql
--!include views/official_assignment.sql
drop function if exists federation_calendar(text[], bigint[], text[], date, date, boolean, integer, integer);
drop function if exists federation_calendar(text[], bigint[], text[], date, date, boolean, boolean, integer, integer);
drop type if exists federation_calendar_day;
drop type if exists federation_calendar_official;
drop type if exists federation_calendar_participation;

--!include functions/competition_suggestions.sql
