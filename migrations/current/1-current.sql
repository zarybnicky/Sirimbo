
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
