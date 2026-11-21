--! Previous: sha1:f78a407474f97e92ab0059048346e2095f7a0c49
--! Hash: sha1:2761974cf18012eef68555626d9bdd59f7b5b8a1

--! split: 1-current.sql
--! Included functions/resend_person_invitation.sql
create or replace function resend_person_invitation(invitation_id bigint)
  returns void
  language plpgsql strict security definer
  set search_path to pg_catalog, public, pg_temp
as $$
declare
  invitation person_invitation;
begin
  select * into invitation from person_invitation
  where id = invitation_id and tenant_id = current_tenant_id()
  for update;

  if not found then
    raise exception 'INVITATION_NOT_FOUND' using errcode = 'P0002';
  end if;
  if invitation.used_at is not null then
    raise exception 'INVITATION_ALREADY_USED' using errcode = '22023';
  end if;

  perform graphile_worker.add_job('send_invitation', json_build_object('id', invitation.id));
end;
$$;

revoke all on function resend_person_invitation from anonymous;
grant execute on function resend_person_invitation to administrator;
--! EndIncluded functions/resend_person_invitation.sql
--! Included policies/event_registration.sql
select app_private.drop_policies('public.event_registration');

create policy current_tenant on event_registration as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_registration to administrator
  using (true);
create policy trainer_insert on event_registration for insert to trainer
  with check (app_private.can_trainer_edit_event(event_id));
create policy trainer_update on event_registration for update to trainer
  using (app_private.can_trainer_edit_event(event_id));
create policy insert_my on event_registration for insert
  with check (person_id = any((select current_person_ids())::bigint[])
    or couple_id = any((select current_couple_ids())::bigint[]));
create policy delete_my on event_registration for delete
  using (person_id = any((select current_person_ids())::bigint[])
    or couple_id = any((select current_couple_ids())::bigint[]));
create policy view_visible_instance on event_registration for select
  using (event_id in (select id from public.event));

grant all on event_registration to anonymous;
--! EndIncluded policies/event_registration.sql

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
      couple_id bigint,
      only_mine boolean,
      relevant_only boolean,
      can_register boolean,
      with_competitions boolean,
      with_birthdays boolean
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

--! Included functions/tstzrange_overlaps.sql
create or replace function app_private.tstzrange_overlaps_internal(tstzrange, anyrange)
  returns boolean
  language internal immutable strict parallel safe leakproof
as 'range_overlaps';

do $$
begin
  if not exists (
    select from pg_catalog.pg_operator
    where oprnamespace = 'app_private'::regnamespace
      and oprname = '&&'
      and oprleft = 'tstzrange'::regtype
      and oprright = 'anyrange'::regtype
  ) then
    create operator app_private.&& (
      leftarg = tstzrange,
      rightarg = anyrange,
      function = app_private.tstzrange_overlaps_internal
    );
  end if;

  -- rangesel recognizes built-in operator OIDs only.
  alter operator app_private.&& (tstzrange, anyrange) set (
    restrict = pg_catalog.matchingsel,
    join = pg_catalog.matchingjoinsel
  );

  -- GiST's range support dispatches on the right operand's declared type.
  -- Keep it polymorphic so strategy 3 uses its range-overlap implementation.
  if not exists (
    select from pg_catalog.pg_amop member
    join pg_catalog.pg_opfamily family on family.oid = member.amopfamily
    join pg_catalog.pg_am method on method.oid = family.opfmethod
    where family.opfnamespace = 'pg_catalog'::regnamespace
      and family.opfname = 'range_ops'
      and method.amname = 'gist'
      and member.amopopr = 'app_private.&&(tstzrange,anyrange)'::regoperator
      and member.amopstrategy = 3
  ) then
    alter operator family pg_catalog.range_ops using gist
      add operator 3 app_private.&& (tstzrange, anyrange);
  end if;
end;
$$;

-- This wrapper inlines to the registered operator, so existing GiST indexes
-- can use the overlap condition before RLS. Put the indexed range first.
create or replace function app_private.tstzrange_overlaps(tstzrange, tstzrange)
  returns boolean
  language sql immutable strict parallel safe leakproof
return $1 operator(app_private.&&) $2;

grant execute on function app_private.tstzrange_overlaps_internal, app_private.tstzrange_overlaps to anonymous;
--! EndIncluded functions/tstzrange_overlaps.sql
--! Included functions/event_facets.sql
drop function if exists event_instances_for_range;
drop function if exists event_facets;
drop type if exists event_instance_range_scope;

create or replace function event_facets(
  start_range timestamptz,
  end_range timestamptz,
  condition event_search_condition default null
) returns table(facet text, value text, count integer)
language sql stable as $$
  with mine as (
    select event_id from event_registration
    where person_id = any((select current_person_ids())::bigint[])
      and registration_status = 'active'
    union all
    select event_id from event_trainer
    where person_id = any((select current_person_ids())::bigint[])
  ), cohort_people as (
    select cohort_id, person_id from current_cohort_membership
  ), candidates as materialized (
    select i.id, i.type, i.location_id
    from event i
    where i.tenant_id = current_tenant_id()
      and start_range is not null
      and app_private.tstzrange_overlaps(i.range, tstzrange(start_range, end_range, '[)'))
      and ((condition).can_register is null or (event_registration_info(i)).can_register = (condition).can_register)
      and ((condition).parent_id is null or i.parent_id = (condition).parent_id)
      and ((condition).couple_id is null or exists (
        select 1 from event_registration r where r.event_id = i.id
          and r.registration_status = 'active' and r.couple_id = (condition).couple_id
      ))
      and (not coalesce((condition).only_mine, false) or i.id in (select event_id from mine))
      and (not coalesce((condition).relevant_only, false) or i.parent_id is null
        or i.id in (select event_id from mine)
        or i.parent_id in (select event_id from mine))
  ), trainers as (
    select event_id, array_agg(person_id) as ids
    from event_trainer
    where event_id = any(array(select id from candidates))
    group by event_id
  ), participants as (
    select r.event_id, array_agg(r.person_id) as ids
    from event_registration r
    left join event_registration parent on parent.id = r.parent_registration_id
    where r.event_id = any(array(select id from candidates))
      and r.registration_status = 'active' and r.person_id is not null
      and ((condition).couple_id is null or parent.couple_id = (condition).couple_id)
    group by r.event_id
  ), event_people as (
    select c.*, coalesce(t.ids, '{}') as trainers,
      case when (condition).couple_id is null then coalesce(t.ids, '{}') || coalesce(p.ids, '{}')
        else coalesce(p.ids, '{}') end as selected_people
    from candidates c
    left join trainers t on t.event_id = c.id
    left join participants p on p.event_id = c.id
  ), matches as (
    select *,
      (condition).types is null or type = any((condition).types) as type_matches,
      (condition).trainer_ids is null or trainers && (condition).trainer_ids as trainer_matches,
      (condition).participant_ids is null or case when (condition).cohort_id is null
        then selected_people && (condition).participant_ids
        else selected_people && array(
          select person_id from cohort_people c
          where c.cohort_id = (condition).cohort_id and person_id = any((condition).participant_ids)
        ) end as participant_matches,
      (condition).cohort_id is null or selected_people && array(
        select person_id from cohort_people c where c.cohort_id = (condition).cohort_id
      ) as cohort_matches,
      (condition).location_ids is null or location_id = any((condition).location_ids) as location_matches
    from event_people
  ), facets as (
    select m.id, 'type' as facet, upper(m.type::text) as value
    from matches m
    where m.trainer_matches and m.participant_matches and m.location_matches and m.cohort_matches
    union all
    select m.id, 'trainer', unnest(m.trainers)::text
    from matches m
    where m.type_matches and m.participant_matches and m.location_matches and m.cohort_matches
    union all
    select m.id, 'participant', person_id::text
    from matches m
    cross join lateral unnest(m.selected_people) person(person_id)
    where m.type_matches and m.trainer_matches and m.location_matches and m.cohort_matches
      and ((condition).cohort_id is null or person_id in (
        select person_id from cohort_people c where c.cohort_id = (condition).cohort_id
      ))
    union all
    select m.id, 'location', m.location_id::text
    from matches m
    where m.type_matches and m.trainer_matches and m.participant_matches and m.cohort_matches
      and m.location_id is not null
    union all
    select m.id, 'cohort', c.cohort_id::text
    from matches m
    cross join lateral unnest(m.selected_people) person(person_id)
    join cohort_people c using (person_id)
    where m.type_matches and m.trainer_matches and m.location_matches
      and ((condition).participant_ids is null or c.person_id = any((condition).participant_ids))
  )
  select facet, value, count(*)::integer
  from (select distinct id, facet, value from facets) events
  group by facet, value;
$$;
comment on function event_facets is '@behavior +list -connection';
grant execute on function event_facets to anonymous;
--! EndIncluded functions/event_facets.sql
--! Included functions/save_payment_group.sql
drop function if exists save_payment_group;
drop type if exists payment_group_item;

create type payment_group_item as (
  person_id bigint,
  due_at timestamptz,
  amount numeric
);

create or replace function save_payment_group(group_id bigint, payments payment_group_item[])
returns setof payment
language plpgsql
as $$
declare
  subscription cohort_subscription;
  item payment_group_item;
  payment_row payment;
  person_row person;
begin
  select * into strict subscription from cohort_subscription where id = group_id for update;

  foreach item in array payments loop
    select p.* into payment_row from payment p join payment_debtor d on d.payment_id = p.id
    where p.cohort_subscription_id = group_id and p.due_at = item.due_at and d.person_id = item.person_id
    for update of p;

    if found then
      update payment_recipient set amount = item.amount
      where payment_id = payment_row.id and amount is distinct from item.amount;
      if not found then
        return next payment_row;
        continue;
      end if;
    else
      select * into strict person_row from person where id = item.person_id;

      insert into payment (
        cohort_subscription_id,
        due_at,
        status,
        variable_symbol,
        specific_symbol,
        is_auto_credit_allowed
      ) values (
        group_id,
        item.due_at,
        case when item.due_at > now() then 'tentative'::payment_status else 'unpaid'::payment_status end,
        regexp_replace(coalesce(nullif(person_row.tax_identification_number, ''), person_row.id::text), '[^0-9]', '', 'g'),
        '' || (extract(year from item.due_at) - 2000) || extract(month from item.due_at) || extract(day from item.due_at) || group_id,
        current_tenant_id() = 2
      )
      returning * into payment_row;

      insert into payment_debtor (payment_id, person_id) values (payment_row.id, item.person_id);
      insert into payment_recipient (payment_id, account_id, amount)
      values (payment_row.id, subscription.account_id, item.amount);
    end if;

    if payment_row.is_auto_credit_allowed then
      -- Keep the payment ID while rebuilding its credit transaction and postings.
      delete from transaction where payment_id = payment_row.id and source = 'auto-credit';
      update payment set
        status = case when due_at > now() then 'tentative'::payment_status else 'unpaid'::payment_status end,
        paid_at = null
      where id = payment_row.id returning * into payment_row;
      payment_row := coalesce(resolve_payment_with_credit(payment_row), payment_row);
    end if;

    return next payment_row;
  end loop;
end;
$$;

revoke all on function save_payment_group from anonymous;
grant execute on function save_payment_group to administrator;
--! EndIncluded functions/save_payment_group.sql
--! Included functions/resolve_payment_with_credit.sql
create or replace function resolve_payment_with_credit(p payment) returns payment
  language plpgsql
as $$
declare
  trans transaction;
  recipient payment_recipient;
  acc account;
  trainer tenant_trainer;
  remaining_amount numeric(19, 4);
  total_amount numeric(19, 4);
  actual_payout numeric(19, 4);
begin
  if p.status <> 'unpaid' or not p.is_auto_credit_allowed then
    return null;
  end if;

  insert into transaction (payment_id, accounting_period_id, source)
  values (p.id, p.accounting_period_id, 'auto-credit')
  returning * into trans;

  total_amount := 0;

  for recipient in select * from payment_recipient where payment_id=p.id loop
      remaining_amount := recipient.amount;
      total_amount := total_amount + remaining_amount;

      select account.* into acc from account where id=recipient.account_id;
      select tenant_trainer.* into trainer from tenant_trainer where acc.person_id=tenant_trainer.person_id and tenant_id=acc.tenant_id;

      if trainer is null or trainer.create_payout_payments then
        if trainer.member_payout_45min_amount is not null then
          actual_payout := remaining_amount * trainer.member_payout_45min_amount / trainer.member_price_45min_amount;
          insert into posting (transaction_id, original_account_id, account_id, amount)
          values (trans.id, recipient.account_id, (select id from tenant_account(acc.currency)), remaining_amount - actual_payout);
          remaining_amount := actual_payout;
        end if;

        insert into posting (transaction_id, account_id, amount)
        values (trans.id, recipient.account_id, remaining_amount);
      end if;
    end loop;

  remaining_amount := total_amount / (select coalesce(nullif(count(*), 0), 1) from payment_debtor where payment_id = p.id);
  for acc in select a.* from payment_debtor d join lateral person_account(d.person_id, 'CZK') a on true where payment_id = p.id loop
      insert into posting (transaction_id, account_id, amount) values (trans.id, acc.id, 0.0 - remaining_amount);
    end loop;

  update payment set status = 'paid', paid_at = now() where id=p.id returning * into p;
  return p;
end
$$;

COMMENT ON FUNCTION resolve_payment_with_credit IS '@behavior -*';

GRANT ALL ON FUNCTION resolve_payment_with_credit TO anonymous;

create or replace function app_private.resolve_due_payments() returns setof payment
  language plpgsql
as $$
declare
  tenant_ids bigint[] := array(select id from tenant);
  v_tenant_id bigint;
  payment_row payment;
begin
  if not row_security_active('payment') then
    set local role to administrator;
  end if;
  foreach v_tenant_id in array tenant_ids loop
    perform set_config('jwt.claims.tenant_id', v_tenant_id::text, true);
    perform set_config('jwt.claims.my_tenant_ids', json_build_array(v_tenant_id)::text, true);
    for payment_row in
      update payment p set status = 'unpaid'
      where p.tenant_id = v_tenant_id and p.cohort_subscription_id is not null
        and p.due_at <= now()
        and (p.status = 'tentative' or (p.status = 'unpaid' and p.is_auto_credit_allowed))
      returning p.*
    loop
      return next coalesce(resolve_payment_with_credit(payment_row), payment_row);
    end loop;
  end loop;
end;
$$;
--! EndIncluded functions/resolve_payment_with_credit.sql
--! Included functions/create_latest_lesson_payments.sql
CREATE or replace FUNCTION app_private.create_latest_lesson_payments() RETURNS SETOF payment
  LANGUAGE plpgsql
AS $$
begin
  return query select * from app_private.resolve_due_payments();

  perform set_config('jwt.claims.tenant_id', '2', true);
  perform set_config('jwt.claims.my_tenant_ids', '[2]', true);
  if not (select row_security_active('event')) then
    set local role to administrator;
  end if;

  return query WITH created AS (
    SELECT p.*
    FROM event ei
      JOIN LATERAL create_event_payment(ei) p ON true
    WHERE ei.type = 'lesson'
      AND NOT ei.is_cancelled
      AND ei.since < now()
      AND NOT EXISTS (
        SELECT 1
        FROM payment p
        WHERE p.event_id = ei.id AND p.status = 'paid'
      )
      AND p.status in ('unpaid', 'tentative')
  ),
  unpaid AS (
    UPDATE payment p
      SET status = 'unpaid'
      FROM created
      WHERE p.id = created.id
      RETURNING p.*
  )
  SELECT p.*
  FROM unpaid
    CROSS JOIN LATERAL resolve_payment_with_credit(unpaid.*) p
  WHERE p IS NOT NULL;
end;
$$;
--! EndIncluded functions/create_latest_lesson_payments.sql

comment on table cohort_subscription is '@behavior +list -connection -insert -update -delete -query:resource:connection -query:resource:single';

alter table couple add column if not exists name text;
alter table couple add column if not exists short_name text;

--! Included functions/tg_couple__names.sql
create or replace function app_private.tg_couple__names() returns trigger
language plpgsql security definer
set search_path = pg_catalog, public, pg_temp as $$
begin
  select
    concat_ws(' - ', concat_ws(' ', man.first_name, man.last_name), concat_ws(' ', woman.first_name, woman.last_name)),
    concat_ws(' - ', man.last_name, woman.last_name)
  into new.name, new.short_name
  from person man, person woman
  where man.id = new.man_id and woman.id = new.woman_id
  -- A concurrent rename must finish before we read, or refresh this couple after our write.
  for share of man, woman;
  return new;
end;
$$;

create or replace function app_private.tg_person__couple_names() returns trigger
language plpgsql security definer
set search_path = pg_catalog, public, pg_temp as $$
begin
  -- Re-run the couple trigger so formatting has one definition.
  update couple set name = name where man_id = new.id or woman_id = new.id;
  return new;
end;
$$;

drop trigger if exists _050_names on couple;
create trigger _050_names
  before insert or update of man_id, woman_id, name, short_name on couple
  for each row execute function app_private.tg_couple__names();

drop trigger if exists _500_couple_names on person;
create trigger _500_couple_names
  after update of first_name, last_name on person
  for each row when (old.first_name is distinct from new.first_name or old.last_name is distinct from new.last_name)
  execute function app_private.tg_person__couple_names();
--! EndIncluded functions/tg_couple__names.sql

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
drop function if exists global_search(text, integer, text[]);
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

--! Included functions/current_person_ids.sql
CREATE or replace FUNCTION current_person_ids() RETURNS bigint[] LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.my_person_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;

COMMENT ON FUNCTION current_person_ids() IS '@behavior -*';
GRANT ALL ON FUNCTION current_person_ids() TO anonymous;

CREATE or replace FUNCTION current_couple_ids() RETURNS bigint[] LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.my_couple_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;

COMMENT ON FUNCTION current_couple_ids() IS '@behavior -*';
GRANT ALL ON FUNCTION current_couple_ids() TO anonymous;

CREATE or replace FUNCTION current_cohort_ids() RETURNS bigint[] LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.my_cohort_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;

COMMENT ON FUNCTION current_cohort_ids() IS '@behavior -*';
GRANT ALL ON FUNCTION current_cohort_ids() TO anonymous;

CREATE or replace FUNCTION my_tenants_array() RETURNS bigint[] LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.my_tenant_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;
COMMENT ON FUNCTION my_tenants_array() IS '@behavior -*';
GRANT ALL ON FUNCTION my_tenants_array() TO anonymous;

CREATE or replace FUNCTION current_admin_tenant_ids() RETURNS bigint[] LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.admin_tenant_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;
COMMENT ON FUNCTION current_admin_tenant_ids() IS '@behavior -*';
GRANT ALL ON FUNCTION current_admin_tenant_ids() TO anonymous;
--! EndIncluded functions/current_person_ids.sql

create index if not exists event_visibility_idx on event (tenant_id, id)
  include (is_public, is_visible, manager_person_ids);

select app_private.drop_policies('public.event');
select app_private.drop_policies('public.event_trainer');
select app_private.drop_policies('public.event_target_cohort');
drop function if exists app_private.accessible_event_ids();
drop function if exists app_private.accessible_event_ids(tsquery);

--! Included functions/current_shared_event_ids.sql
create or replace function app_private.current_shared_event_ids() returns bigint[]
language sql stable security definer
set search_path = pg_catalog, public, pg_temp
as $$
  select case
    when nullif(current_setting('jwt.claims.shared.event_token', true), '') is not null then
      coalesce((select instance_ids from app_private.event_share_claims(
        current_tenant_id(), current_setting('jwt.claims.shared.event_token', true)
      )), '{}'::bigint[])
    else '{}'::bigint[]
  end;
$$;
--! EndIncluded functions/current_shared_event_ids.sql
--! Included functions/accessible_event_ids.sql
create or replace function app_private.accessible_event_ids(
  title_query tsquery default null, event_ids bigint[] default null
) returns setof bigint
language plpgsql stable security definer
set search_path = pg_catalog, public, pg_temp
-- Plan for the supplied filters, including index lookups for small ID lists.
set plan_cache_mode = force_custom_plan
as $$
begin
  return query
  with recursive attended(id) as (
    select event_id from public.event_registration
    where tenant_id = (select public.current_tenant_id())
      and person_id = any((select public.current_person_ids())::bigint[])
      and registration_status = 'active'
    union
    select child.id from public.event child
    join attended parent on child.parent_id = parent.id
    where child.parent_id is not null
      and child.tenant_id = (select public.current_tenant_id())
  )
  select id from public.event
  where tenant_id = (select public.current_tenant_id())
    and (event_ids is null or id = any(event_ids))
    and (title_query is null or search_vector @@ title_query)
    and (
      (select pg_has_role(current_setting('role'), 'administrator', 'member'))
      or is_public
      or (is_visible and (select pg_has_role(current_setting('role'), 'member', 'member')))
      or ((select pg_has_role(current_setting('role'), 'trainer', 'member')) and cardinality(manager_person_ids) = 0)
      or (manager_person_ids && (select current_person_ids()))
      or id = any ((select app_private.current_shared_event_ids())::bigint[])
      or id in (select id from attended)
    );
end;
$$;

grant execute on function app_private.accessible_event_ids to anonymous;
--! EndIncluded functions/accessible_event_ids.sql
--! Included functions/global_search.sql
create or replace function global_search(search text, "limit" integer default 20, scope text[] default null)
  returns setof global_search_result
  language sql stable
as $$
  with input as materialized (
    select normalized.text, parsed.words, parsed.prefix
    from (select btrim(regexp_replace(app_private.normalize_name(coalesce(search, '')), '\s+', ' ', 'g')) as text) normalized
    cross join lateral (
      select string_agg(array_to_tsvector(array[token])::text, ' & ')::tsquery as words,
        string_agg(array_to_tsvector(array[token])::text || ':*', ' & ')::tsquery as prefix
      from unnest(tsvector_to_array(to_tsvector('pg_catalog.simple', normalized.text))) token
    ) parsed
    where char_length(normalized.text) >= 3 and parsed.prefix is not null
  ), person_recency as (
    -- Rank by the latest club relationship; pending relationships do not count.
    select person_id, max(coalesce(until, 'infinity'::timestamptz)) as until
    from (
      select person_id, until from tenant_membership where tenant_id = current_tenant_id() and status <> 'pending' and since <= now()
      union all
      select person_id, until from tenant_trainer where tenant_id = current_tenant_id() and status <> 'pending' and since <= now()
      union all
      select person_id, until from tenant_administrator where tenant_id = current_tenant_id() and status <> 'pending' and since <= now()
    ) relationships
    group by person_id
  ), peers as materialized (
    select person_id from current_cohort_membership
    where cohort_id = any((select current_cohort_ids())::bigint[])
  ), event_matches as materialized (
    select e.*
    from event e
    where (scope is null or 'event' = any(scope)) and e.id = any (array(
      select ids.id from input q
      cross join lateral app_private.accessible_event_ids(q.prefix) ids(id)
    ))
  ), candidates as (
    select p as person, null::couple as couple, null::cohort as cohort,
      null::location as location, null::event as event,
      p.id, p.search_name as normalized_label, p.search_vector,
      (case when p.id = any((select current_person_ids())::bigint[]) then 20
        when p.id in (select person_id from peers) then 10 else 0 end
        + case when r.person_id is null then 0 else 10 / (1 + greatest(0, extract(epoch from now() - r.until)) / 86400 / 365) end)::real as boost
    from person p
    left join person_recency r on r.person_id = p.id
    cross join input q
    where (scope is null or 'person' = any(scope)) and p.search_vector @@ q.prefix
    union all
    select null, c, null, null, null, c.id, app_private.normalize_name(c.name), c.search_vector,
      case when c.id = any((select current_couple_ids())::bigint[]) then 20 else 0 end
        + 10 / (1 + greatest(0, extract(epoch from now() - coalesce(c.until, 'infinity'::timestamptz))) / 86400 / 365)
    from couple c
    cross join input q
    where (scope is null or 'couple' = any(scope)) and c.search_vector @@ q.prefix
    union all
    select null, null, c, null, null, c.id, app_private.normalize_name(c.name), c.search_vector,
      case when c.id = any((select current_cohort_ids())::bigint[]) then 20 else 0 end
        + case when not c.is_archived then 10 else 0 end
    from cohort c, input q
    where (scope is null or 'cohort' = any(scope)) and c.search_vector @@ q.prefix
    union all
    select null, null, null, l, null, l.id, app_private.normalize_name(l.name), l.search_vector, 0
    from location l, input q
    where (scope is null or 'location' = any(scope)) and l.search_vector @@ q.prefix
    union all
    select null, null, null, null, e::event, e.id,
      app_private.normalize_name(e.name), e.search_vector,
      case when e.manager_person_ids && (select current_person_ids())
        or (event_registration_info(e::event)).my then 20 else 0 end
      -- Upcoming/ongoing: up to 10; past: up to 5. Both decay over a 90-day scale.
      + (case when e.until >= now()
        then 10 / (1 + greatest(0, extract(epoch from e.since - now())) / 86400 / 90)
        else 5 / (1 + extract(epoch from now() - e.until) / 86400 / 90) end)::real
    from event_matches e
  ), scored as (
    select c.*,
      case when btrim(regexp_replace(c.normalized_label, '\s+', ' ', 'g')) = q.text then 0
        when c.search_vector @@ q.words then 1 else 2 end as match_tier,
      (c.boost + 5 * ts_rank(c.search_vector, q.prefix, 32)
        - case when (c.event).type = 'lesson' then 20 else 0 end)::real as score
    from candidates c, input q
  )
  select person, couple, cohort, location, event, score,
    row_number() over (order by match_tier, score desc, id)::integer
  from scored
  order by match_tier, score desc, id
  limit greatest(0, least(coalesce("limit", 20), 20));
$$;

comment on function global_search is '@behavior +list -connection
@notNull';
grant execute on function global_search to anonymous;
--! EndIncluded functions/global_search.sql
--! Included policies/event.sql
select app_private.drop_policies('public.event');

create policy current_tenant on event as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_same_tenant on event to administrator
  using (true);
-- INSERT ... RETURNING needs a row-local grant: the stable ID helper cannot see the new row yet.
create policy trainer_select on event for select to trainer
  using (cardinality(manager_person_ids) = 0 or manager_person_ids && (select current_person_ids()));
create policy trainer_insert on event for insert to trainer
  with check (parent_id is null or app_private.can_trainer_edit_event(parent_id));
create policy trainer_update on event for update to trainer
  using (app_private.can_trainer_edit_event(id));
create policy trainer_delete on event for delete to trainer
  using (app_private.can_trainer_edit_event(id));
-- Let ordinary reads avoid building the full accessible-ID set.
create policy member_view on event for select to member
  using (is_visible);
create policy public_view on event for select to anonymous
  using (is_public);
create policy event_access_view on event for select to anonymous
  using (id in (select * from app_private.accessible_event_ids()));
--! EndIncluded policies/event.sql
--! Included policies/event_trainer.sql
select app_private.drop_policies('public.event_trainer');

create policy current_tenant on event_trainer as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_trainer to administrator
  using (true);
create policy trainer_same_tenant on event_trainer to trainer
  using (app_private.can_trainer_edit_event(event_id));
create policy member_view on event_trainer for select to member
  using (true);
create policy public_view on event_trainer for select to anonymous
  using (event_id in (select id from event where is_public));
create policy event_access_view on event_trainer for select to anonymous
  using (event_id in (select * from app_private.accessible_event_ids()));
--! EndIncluded policies/event_trainer.sql
--! Included policies/event_target_cohort.sql
select app_private.drop_policies('public.event_target_cohort');

create policy current_tenant on event_target_cohort as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_target_cohort to administrator
  using (true);
create policy trainer_select on event_target_cohort to trainer
  using (app_private.can_trainer_edit_event(event_id));
create policy member_view on event_target_cohort for select to member
  using (exists (select 1 from event e where e.id = event_target_cohort.event_id));
create policy event_access_view on event_target_cohort for select to anonymous
  using (event_id in (select * from app_private.accessible_event_ids()));
--! EndIncluded policies/event_target_cohort.sql
--! Included functions/cohort_has_accessible_event.sql
create or replace function app_private.cohort_has_accessible_event(p_cohort_id bigint)
  returns boolean
  language sql stable security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  -- Most cohorts have a public or member-visible event. Check their indexed
  -- links before enumerating the caller's additional event access.
  select exists (
    select 1
    from event_target_cohort target
    join event e on e.tenant_id = target.tenant_id and e.id = target.event_id
    where target.tenant_id = (select current_tenant_id())
      and target.cohort_id = p_cohort_id
      and (e.is_public or (e.is_visible
        and (select pg_has_role(current_setting('role'), 'member', 'member'))))
  ) or exists (
    with targets as materialized (
      select event_id from event_target_cohort
      where tenant_id = (select current_tenant_id()) and cohort_id = p_cohort_id
    )
    select 1 from targets
    where event_id = any (array(select * from app_private.accessible_event_ids()))
  );
$$;

grant execute on function app_private.cohort_has_accessible_event(bigint) to anonymous;
--! EndIncluded functions/cohort_has_accessible_event.sql
--! Included functions/visible_file_ids.sql
create or replace function app_private.visible_file_ids()
  returns setof bigint
  language sql stable
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  -- Start with cover candidates rather than probing every accessible event.
  with event_covers as materialized (
    select id, cover_image_id from event
    where tenant_id = (select current_tenant_id())
      and cover_image_id is not null
  )
  select id
  from file
  where tenant_id = (select current_tenant_id())
    and is_public

  union all

  select f.file_id
  from announcement_attachment f
  join app_private.visible_announcement_ids() a(id) on a.id = f.announcement_id
  where f.tenant_id = (select current_tenant_id())

  union all

  select f.file_id
  from article_attachment f
  join aktuality a on id = f.aktuality_id and a.tenant_id = f.tenant_id
  where a.tenant_id = (select current_tenant_id())
    and a.is_visible

  union all

  select image.file_id
  from location_image image
  join location
    on location.tenant_id = image.tenant_id
    and location.id = image.location_id
  where image.tenant_id = (select current_tenant_id())

  union all

  select cover_image_id
  from location
  where tenant_id = (select current_tenant_id())
    and cover_image_id is not null

  union all

  select cover_image_id from event_covers
  where id = any (array(
    select * from app_private.accessible_event_ids(null, array(select id from event_covers))
  ));
$$;

grant execute on function app_private.visible_file_ids() to anonymous;
--! EndIncluded functions/visible_file_ids.sql

--! Included views/competition_participation.sql
create or replace view federated.competition_participation
with (security_invoker = true) as
select r.competition_id, r.competitor_id, coalesce(e.cancelled, false) as cancelled,
  r.ranking, r.ranking_to, r.point_gain, r.final_gain, r.is_final, r.completion_status,
  c.event_id, c.start_date as date, r.start_number
from federated.competition_result r
join federated.competition c on c.id = r.competition_id
left join federated.competition_entry e using (competition_id, competitor_id)
union all
select e.competition_id, e.competitor_id, e.cancelled,
  null::integer, null::integer, null::numeric(10,3), null::numeric(10,3), null::boolean, null::text,
  c.event_id, c.start_date, null::text
from federated.competition_entry e
join federated.competition c on c.id = e.competition_id
where not exists (
  select 1 from federated.competition_result r
  where (r.competition_id, r.competitor_id) = (e.competition_id, e.competitor_id)
);

comment on view federated.competition_participation is '@name competition_participation
@primaryKey competition_id,competitor_id
@foreignKey (event_id) references federated.event (id)|@fieldName event|@foreignFieldName participations|@notNull
@foreignKey (competition_id) references federated.competition (id)|@fieldName competition|@foreignFieldName participations|@notNull
@foreignKey (competitor_id) references federated.competitor (id)|@fieldName competitor|@foreignFieldName competitionParticipations|@notNull
@behavior -single -list -connection +manyRelation:resource:list -insert -update -delete';
grant select on federated.competition_participation to anonymous;
comment on column federated.competition_participation.competition_id is '@notNull';
comment on column federated.competition_participation.competitor_id is '@notNull';
comment on column federated.competition_participation.cancelled is '@notNull';

comment on column federated.competition_participation.event_id is '@notNull';
comment on column federated.competition_participation.date is '@notNull';
--! EndIncluded views/competition_participation.sql
--! Included views/official_assignment.sql
create or replace view federated.official_assignment
with (security_invoker = true) as
select c.event_id, c.start_date as date, o.person_id, o.role
from federated.competition_official o
join federated.competition c on c.id = o.competition_id
where c.status <> 'cancelled'
union
select o.event_id, e.start_date, o.person_id, o.role
from federated.event_official o
join federated.event e on e.id = o.event_id;

comment on view federated.official_assignment is '@name official_assignment
@primaryKey event_id,date,person_id,role
@foreignKey (event_id) references federated.event (id)|@fieldName event|@foreignFieldName officialAssignments|@notNull
@foreignKey (person_id) references federated.person (id)|@fieldName person|@notNull
@behavior -single -list -connection +manyRelation:resource:list -insert -update -delete';
grant select on federated.official_assignment to anonymous;

comment on column federated.official_assignment.event_id is '@notNull';
comment on column federated.official_assignment.date is '@notNull';
comment on column federated.official_assignment.person_id is '@notNull';
comment on column federated.official_assignment.role is '@notNull';
--! EndIncluded views/official_assignment.sql
--! Included functions/competition_suggestions.sql
drop function if exists competition_suggestions;
drop function if exists suggested_competition_matches;

create or replace function suggested_competition_matches(
  since date,
  until date default null,
  person_ids bigint[] default null,
  couple_id bigint default null,
  cohort_id bigint default null,
  event_ids bigint[] default null,
  registered_only boolean default false
) returns table (competition_id bigint, competitor_id text, local_person_id bigint, local_couple_id bigint, status text)
language sql stable as $$
  with scoped_people as materialized (
    select p.id
    from person p
    where (person_ids is not null or couple_id is not null or p.id in (
        select person_id from current_tenant_membership
        union all
        select person_id from current_tenant_trainer
        union all
        select person_id from current_tenant_administrator
      ))
      and (person_ids is null or p.id = any(person_ids))
      and (cohort_id is null or exists (
        select 1 from current_cohort_membership cm
        where cm.person_id = p.id and cm.cohort_id = suggested_competition_matches.cohort_id
      ))
      and (couple_id is null or exists (
        select 1 from couple c
        where c.id = suggested_competition_matches.couple_id and p.id in (c.man_id, c.woman_id)
      ))
  ), scoped_couples as materialized (
    select c.id
    from couple c
    where c.status = 'active'
      and (couple_id is null or c.id = couple_id)
      and (c.man_id in (select id from scoped_people) or c.woman_id in (select id from scoped_people))
  ), scoped_competitors as materialized (
    select fc.id, fc.federation, fc.competitor_type, array(
      select distinct case
        when license.discipline::text like 'pd_%' then 'Professional'
        when license.discipline = 'hiphop' then 'HipHop'
        when license.discipline in ('breaking', 'caribbean', 'stage', 'smooth', 'disco')
          then initcap(license.discipline::text)
        else 'General'
      end
      from federated.person_license license
      where license.person_id = fp.id and license.federation = 'wdsf'
        and license.kind = 'athlete' and license.discipline <> 'unknown'
    ) as divisions, fp.age_group_code, link.person_id as local_person_id, null::bigint as local_couple_id
    from person_federation_link link
    join federated.person fp on (fp.federation, fp.external_id) = (link.federation, link.external_id)
    join federated.competitor_component member on member.person_id = fp.id
    join federated.competitor fc on fc.id = member.competitor_id
    where couple_id is null and fc.competitor_type = 'solo'
      and link.person_id = any(array(select id from scoped_people))
    union
    select fc.id, fc.federation, fc.competitor_type, fc.divisions, fc.age_group_code,
      null::bigint, link.couple_id
    from couple_federation_link link
    join federated.competitor fc on fc.id = link.competitor_id
    where link.couple_id = any(array(select id from scoped_couples))
  ), profiles as materialized (
    select sc.local_person_id, sc.local_couple_id, sc.id, sc.federation, sc.competitor_type, sc.divisions, sc.age_group_code,
      count(*) = count(coalesce(extract(year from p.birth_date)::int, extract(year from fp.dob)::int, fp.birth_year)) as known_birth_years,
      min(coalesce(extract(year from p.birth_date)::int, extract(year from fp.dob)::int, fp.birth_year)) as older_birth_year,
      max(coalesce(extract(year from p.birth_date)::int, extract(year from fp.dob)::int, fp.birth_year)) as younger_birth_year,
      bool_and(coalesce(fp.gender = 'female', false)) as all_female,
      bool_and(coalesce(fp.gender = 'male', false)) as all_male
    from scoped_competitors sc
    join federated.competitor_component cc on cc.competitor_id = sc.id
    join federated.person fp on fp.id = cc.person_id
    left join person_federation_link link on (link.federation, link.external_id) = (fp.federation, fp.external_id)
    left join person p on p.id = link.person_id
    group by sc.local_person_id, sc.local_couple_id, sc.id, sc.federation, sc.competitor_type, sc.divisions, sc.age_group_code
  ), upcoming as materialized (
    select array_agg(c.id) as competition_ids, c.federation, c.competition_type, extract(year from c.start_date)::int as year,
      cat.series, cat.discipline, cat.class, cat.gender_group, cat.competitor_type, cat.age_group_code
    from federated.competition c
    join federated.event e on e.id = c.event_id
    join federated.category cat on cat.id = c.category_id
    where c.federation in ('csts', 'wdsf') and c.start_date >= since and (until is null or c.start_date < until)
      and (event_ids is null or c.event_id = any(event_ids))
      and c.status <> 'cancelled' and e.status <> 'cancelled'
    group by c.federation, c.competition_type, extract(year from c.start_date)::int, cat.id
  ), participations as materialized (
    select p.competition_id, p.competitor_id, p.cancelled
    from federated.competition_participation p
    where p.competition_id = any(array(select unnest(competition_ids) from upcoming))
      and p.competitor_id = any(array(select id from scoped_competitors))
  ), source_profiles as materialized (
    select p.*, source.series, source.discipline, source.class, source.age_group_code as reported_age
    from profiles p
    join federated.competitor_category_progress progress on progress.competitor_id = p.id
    join federated.category source on source.id = progress.category_id
      and source.competitor_type = p.competitor_type
    where p.federation = 'csts'
    union all
    -- WDSF divisions are independent of ČSTS proficiency classes and progress.
    select p.*, division, null, null, p.age_group_code
    from profiles p cross join unnest(p.divisions) division
    where p.federation = 'wdsf'
  ), category_profiles as materialized (
    select p.*, y.year, age.code as age_group, age.allowed_to_dance_in
    from source_profiles p
    cross join (select distinct year from upcoming) y
    left join lateral (
      select r.age_group_code as code
      from federated.couple_age_rule r
      where p.competitor_type = 'couple' and p.known_birth_years
        and r.division = 'General' and r.age_group_code <> 'Under_21'
        and r.older_age @> (y.year - p.older_birth_year)
        and r.younger_age @> (y.year - p.younger_birth_year)
      order by lower(r.older_age) desc nulls last, upper(r.older_age)
      limit 1
    ) couple_age on true
    left join federated.age_group person_age on p.competitor_type = 'solo' and p.known_birth_years
      and person_age.person_age @> (y.year - p.older_birth_year)
    join federated.age_group age on age.code = case
      when p.known_birth_years then coalesce(couple_age.code, person_age.code)
      when y.year = extract(year from current_date)::int then p.reported_age
    end
  ), eligible as (
  select distinct competition_id, p.id, p.local_person_id, p.local_couple_id
  from upcoming target
  join category_profiles p on p.year = target.year and p.federation = target.federation
    and (p.series = target.series or (p.federation = 'wdsf' and target.series = 'Any'))
    and (p.federation = 'wdsf' or p.discipline = target.discipline)
    and p.competitor_type = target.competitor_type
  cross join lateral unnest(target.competition_ids) competition_id
  where (target.gender_group in ('', 'mixed', 'SOLO')
      or (target.gender_group in ('female', 'F', '1 F') and p.all_female)
      or (target.gender_group in ('male', 'M', '1 M') and p.all_male))
    -- ČSTS leagues require B or higher, even though their categories say Open.
    and (p.federation <> 'csts' or target.competition_type not in ('league', 'super_league')
      or p.class in ('B', 'A', 'M', 'S'))
    and (p.federation = 'wdsf' or
      target.class in ('', 'Open') or target.class = p.class
      or (federated.class_rank(p.class) > 0
        and federated.class_rank(p.class) = federated.class_rank(target.class))
      or (target.class like '%-%'
        and federated.class_rank(split_part(target.class, '-', 1)) > 0
        and federated.class_rank(split_part(target.class, '-', 2)) > 0
        -- A range cannot cross between medal and letter classes.
        and (federated.class_rank(split_part(target.class, '-', 1)) > 10)
          = (federated.class_rank(split_part(target.class, '-', 2)) > 10)
        and federated.class_rank(p.class) between
          federated.class_rank(split_part(target.class, '-', 1)) and
          federated.class_rank(split_part(target.class, '-', 2)))
    )
    and (target.age_group_code = p.age_group or target.age_group_code = any(p.allowed_to_dance_in))
    -- Adult's allowances include U21; that is only valid below its upper age limit.
    and (target.age_group_code <> 'Under_21' or
      (p.known_birth_years and target.year - p.older_birth_year < 21) or
      (not p.known_birth_years and p.age_group in ('Youth', 'Under_21')))
  )
  select eligible.*, case
    when participation.cancelled then 'cancelled'
    when participation.competition_id is not null then 'registered'
    else 'suggested'
  end
  from eligible
  left join participations participation
    on (participation.competition_id, participation.competitor_id) = (eligible.competition_id, eligible.id)
  where not registered_only
  union
  select participation.competition_id, sc.id, sc.local_person_id, sc.local_couple_id,
    case when participation.cancelled then 'cancelled' else 'registered' end
  from scoped_competitors sc
  join participations participation on participation.competitor_id = sc.id
  where not registered_only or not participation.cancelled;
$$;

comment on function suggested_competition_matches is '@behavior -*';
grant execute on function suggested_competition_matches to anonymous;

-- Return the local competitors as rows so callers can summarize or inspect them.
create or replace function competition_suggestions(
  event_ids bigint[],
  since date default current_date,
  until date default null,
  person_ids bigint[] default null,
  couple_id bigint default null,
  cohort_id bigint default null,
  registered_only boolean default false
) returns table (
  competition federated.competition,
  person public.person,
  couple public.couple,
  status text
)
language plpgsql stable as $$
begin
  if event_ids is null then
    raise exception 'event_ids is required' using errcode = '22004';
  end if;
  return query
  select c, p, partnership, m.status
  from (
    select competition_id, local_person_id, local_couple_id, case
      when bool_or(matches.status = 'registered') then 'registered'
      when bool_or(matches.status = 'cancelled') then 'cancelled'
      else 'suggested'
    end as status
    from suggested_competition_matches(since, until, person_ids, couple_id, cohort_id, event_ids, registered_only)
      matches
    group by competition_id, local_person_id, local_couple_id
  ) m
  join federated.competition c on c.id = m.competition_id
  left join public.person p on p.id = m.local_person_id
  left join public.couple partnership on partnership.id = m.local_couple_id;
end;
$$;

comment on function competition_suggestions is '@behavior +list -connection';
grant execute on function competition_suggestions to anonymous;
--! EndIncluded functions/competition_suggestions.sql

alter table public.tenant add column if not exists created_at timestamptz;
alter table public.tenant alter column created_at set default now();
comment on column public.tenant.created_at is '@behavior -attribute:insert -attribute:update';

-- Estimate historical tenant creation from the earliest related record creation.
update public.tenant t
set created_at = history.created_at
from (
  select tenant_id, min(created_at) as created_at
  from (
    select tenant_id, created_at from public.tenant_membership
    union all
    select tenant_id, created_at from public.tenant_administrator
    union all
    select tenant_id, created_at from public.tenant_trainer
    union all
    select tenant_id, created_at from public.event
  ) records
  group by tenant_id
) history
where t.id = history.tenant_id and t.created_at is null;

update membership_application
set prefix_title = coalesce(prefix_title, ''), suffix_title = coalesce(suffix_title, ''), bio = coalesce(bio, '')
where prefix_title is null or suffix_title is null or bio is null;
alter table membership_application
  alter column prefix_title set default '', alter column prefix_title set not null,
  alter column suffix_title set default '', alter column suffix_title set not null,
  alter column bio set default '', alter column bio set not null;

--! Included functions/users_claims.sql
create or replace function users_claims(u users) returns jsonb
  language sql stable strict
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select to_jsonb(app_private.create_jwt_token(actual_user)) - 'exp' - 'tenant_id'
  from users actual_user
  where actual_user.id = u.id
    and (u.id = current_user_id()
      or pg_has_role(current_setting('role'), 'administrator', 'member'));
$$;

comment on function users_claims is '@behavior +typeField -queryField';
grant execute on function users_claims to anonymous;
--! EndIncluded functions/users_claims.sql
--! Included functions/current_person_ids.sql
CREATE or replace FUNCTION current_person_ids() RETURNS bigint[] LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.my_person_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;

COMMENT ON FUNCTION current_person_ids() IS '@behavior -*';
GRANT ALL ON FUNCTION current_person_ids() TO anonymous;

CREATE or replace FUNCTION current_couple_ids() RETURNS bigint[] LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.my_couple_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;

COMMENT ON FUNCTION current_couple_ids() IS '@behavior -*';
GRANT ALL ON FUNCTION current_couple_ids() TO anonymous;

CREATE or replace FUNCTION current_cohort_ids() RETURNS bigint[] LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.my_cohort_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;

COMMENT ON FUNCTION current_cohort_ids() IS '@behavior -*';
GRANT ALL ON FUNCTION current_cohort_ids() TO anonymous;

CREATE or replace FUNCTION my_tenants_array() RETURNS bigint[] LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.my_tenant_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;
COMMENT ON FUNCTION my_tenants_array() IS '@behavior -*';
GRANT ALL ON FUNCTION my_tenants_array() TO anonymous;

CREATE or replace FUNCTION current_admin_tenant_ids() RETURNS bigint[] LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.admin_tenant_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;
COMMENT ON FUNCTION current_admin_tenant_ids() IS '@behavior -*';
GRANT ALL ON FUNCTION current_admin_tenant_ids() TO anonymous;
--! EndIncluded functions/current_person_ids.sql
--! Included functions/can_edit_person.sql
create or replace function app_private.can_edit_person(p_person_id bigint)
  returns boolean
  language sql stable security definer leakproof parallel safe
  set search_path = pg_catalog, public, pg_temp
as $$
  select exists (
    select 1 from tenant_membership
    where person_id = p_person_id
      and tenant_id = any ((select current_admin_tenant_ids())::bigint[])
    union all
    select 1 from tenant_trainer
    where person_id = p_person_id
      and tenant_id = any ((select current_admin_tenant_ids())::bigint[])
    union all
    select 1 from tenant_administrator
    where person_id = p_person_id
      and tenant_id = any ((select current_admin_tenant_ids())::bigint[])
  );
$$;

grant all on function app_private.can_edit_person to anonymous;
--! EndIncluded functions/can_edit_person.sql
--! Included functions/visible_person_ids.sql
create or replace function app_private.visible_person_ids(include_partners boolean default false)
returns setof bigint
language sql stable leakproof parallel safe security definer
set search_path = pg_catalog, public, pg_temp
as $$
  with related(person_id) as materialized (
    select unnest(current_person_ids())
    union all
    select person_id from tenant_trainer
    where tenant_id = any ((select current_admin_tenant_ids())::bigint[])
      or (tenant_id = (select current_tenant_id()) and status = 'active')
    union all
    select person_id from tenant_administrator
    where tenant_id = any ((select current_admin_tenant_ids())::bigint[])
      or (tenant_id = (select current_tenant_id()) and status = 'active')
    union all
    select person_id from tenant_membership
    where tenant_id = any ((select current_admin_tenant_ids())::bigint[])
      or (tenant_id = (select current_tenant_id()) and status = 'active'
        and (select current_tenant_id() = any(my_tenants_array())))
    union all
    select visible_person_id from app_private.event_people
    where tenant_id = (select current_tenant_id())
      and viewer_person_id = any((select current_person_ids())::bigint[])
  ), visible(person_id) as materialized (
    select person_id from related
    union all
    select p.id from person p
    where (select cardinality(current_admin_tenant_ids()) > 0)
      and p.id not in (select person_id from related)
      and (exists (
        select from event_registration r
        where r.person_id = p.id and r.registration_status = 'active'
          and r.tenant_id = any ((select current_admin_tenant_ids())::bigint[])
      ) or exists (
        select from event_trainer t
        where t.person_id = p.id
          and t.tenant_id = any ((select current_admin_tenant_ids())::bigint[])
      ))
  )
  select person_id from visible
  union all
  select unnest(array[c.man_id, c.woman_id]) from couple c
  where include_partners and c.status = 'active'
    and (c.man_id in (select person_id from visible)
      or c.woman_id in (select person_id from visible)
      or c.id in (
        select couple_id from event_registration
        where tenant_id = (select current_tenant_id())
          and registration_status = 'active' and parent_registration_id is null and couple_id is not null
          and event_id = any ((select app_private.current_shared_event_ids())::bigint[])
      ));
$$;

grant all on function app_private.visible_person_ids(boolean) to anonymous;
--! EndIncluded functions/visible_person_ids.sql
--! Included functions/create_person.sql
drop function if exists create_person;

create or replace function create_person(
  person_id bigint,
  inout p person,
  is_member boolean,
  is_trainer boolean,
  is_admin boolean,
  send_invitation boolean,
  join_date timestamptz
)
  returns person
  language plpgsql security definer
  set search_path = pg_catalog, public, pg_temp
as $$
begin
  if not pg_has_role(current_setting('role'), 'system_admin', 'member') then
    if not current_tenant_id() = any (current_admin_tenant_ids())
      or (person_id is not null and not app_private.can_edit_person(person_id)) then
      raise insufficient_privilege using message = 'Cannot add this person to this club';
    end if;
    if person_id is null and (is_member or is_trainer or is_admin) is not true then
      raise insufficient_privilege using message = 'A new person must have a club relationship';
    end if;
  end if;
  if person_id is null then
    insert into person (
      first_name,
      last_name,
      gender,
      birth_date,
      nationality,
      tax_identification_number,
      national_id_number,
      csts_id,
      wdsf_id,
      prefix_title,
      suffix_title,
      bio,
      email,
      phone,
      external_ids,
      instagram_username,
      tiktok_username,
      facebook_url,
      website_url
    ) values (
      p.first_name,
      p.last_name,
      p.gender,
      p.birth_date,
      p.nationality,
      p.tax_identification_number,
      p.national_id_number,
      p.csts_id,
      p.wdsf_id,
      p.prefix_title,
      p.suffix_title,
      p.bio,
      p.email,
      p.phone,
      p.external_ids,
      p.instagram_username,
      p.tiktok_username,
      p.facebook_url,
      p.website_url
    ) returning * into p;
  else
    select * into p from person where person.id=person_id;
  end if;
  if is_member = true then
    insert into tenant_membership (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_trainer = true then
    insert into tenant_trainer (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if is_admin = true then
    insert into tenant_administrator (person_id, tenant_id, since) values (p.id, current_tenant_id(), join_date);
  end if;
  if send_invitation = true and p.email is not null and p.email <> '' then
    insert into person_invitation (person_id, tenant_id, email) values (p.id, current_tenant_id(), p.email);
  end if;
end
$$;

grant all on function create_person to anonymous;
--! EndIncluded functions/create_person.sql
--! Included functions/confirm_membership_application.sql
drop function if exists confirm_membership_application(bigint);

create or replace function confirm_membership_application(
  application_id bigint,
  is_member boolean default true,
  is_trainer boolean default false,
  is_admin boolean default false,
  join_date timestamptz default now(),
  cohort_ids bigint[] default array[]::bigint[]
)
  returns person
  language plpgsql security definer
  set search_path = pg_catalog, public, pg_temp
as $$
declare
  result person;
begin
  if not pg_has_role(current_setting('role'), 'administrator', 'member') then
    raise insufficient_privilege using message = 'Cannot confirm this application';
  end if;
  if (is_member or is_trainer or is_admin) is not true then
    raise insufficient_privilege using message = 'A new person must have a club relationship';
  end if;
  if exists (
    select from unnest(cohort_ids) selected(id)
    where not exists (select from cohort where cohort.id = selected.id and tenant_id = current_tenant_id())
  ) then
    raise insufficient_privilege using message = 'Cannot add to another club''s cohort';
  end if;

  with application as materialized (
    select *
    from membership_application
    where id = application_id and status = 'sent' and tenant_id = current_tenant_id()
    for update
  ), t_person as (
    insert into person (
      first_name, last_name, gender, birth_date, nationality, tax_identification_number,
      national_id_number, csts_id, wdsf_id, prefix_title, suffix_title, bio, email, phone,
      note
    )
    select
      first_name, last_name, gender, birth_date, nationality, tax_identification_number,
      national_id_number, csts_id, wdsf_id,
      prefix_title, suffix_title, bio,
      email, phone, note
    from application
    returning *
  ), appl as (
    update membership_application
    set status = 'approved'
    where id = (select id from application)
  ), member as (
    insert into tenant_membership (tenant_id, person_id, since)
    select current_tenant_id(), id, join_date from t_person where is_member
  ), trainer as (
    insert into tenant_trainer (tenant_id, person_id, since)
    select current_tenant_id(), id, join_date from t_person where is_trainer
  ), administrator as (
    insert into tenant_administrator (tenant_id, person_id, since)
    select current_tenant_id(), id, join_date from t_person where is_admin
  ), cohorts as (
    insert into cohort_membership (cohort_id, person_id, since)
    select cohort_id, t_person.id, join_date
    from t_person
    cross join unnest(cohort_ids) selected(cohort_id)
  ), proxy as (
    insert into user_proxy (person_id, user_id)
    select t_person.id, application.created_by
    from t_person cross join application
  )
  select * into result from t_person;
  return result;
end
$$;

grant all on function confirm_membership_application to administrator;
--! EndIncluded functions/confirm_membership_application.sql
--! Included policies/person.sql
select app_private.drop_policies('public.person');

create policy system_admin_all on person to system_admin using (true);
create policy admin_update on person for update to administrator
  using (app_private.can_edit_person(id));
create policy admin_delete on person for delete to administrator
  using (app_private.can_edit_person(id));
create policy admin_myself on person for update
  using (id = any ((select current_person_ids())::bigint[]));
create policy view_tenant_or_trainer on person for select
  using (id in (select app_private.visible_person_ids(true)));

create policy event_share_view on person for select to anonymous
  using (id in (
    select person_id from event_registration
    where registration_status = 'active' and person_id is not null
      and event_id = any ((select app_private.current_shared_event_ids())::bigint[])
    union all
    select person_id from event_trainer
    where event_id = any ((select app_private.current_shared_event_ids())::bigint[])
  ));
--! EndIncluded policies/person.sql
--! Included policies/couple.sql
select app_private.drop_policies('public.couple');

create policy system_admin_all on couple to system_admin using (true);
create policy admin_insert on couple for insert to administrator
  with check (app_private.can_edit_person(man_id) or app_private.can_edit_person(woman_id));
create policy admin_update on couple for update to administrator
  using (app_private.can_edit_person(man_id) or app_private.can_edit_person(woman_id));
create policy admin_delete on couple for delete to administrator
  using (app_private.can_edit_person(man_id) or app_private.can_edit_person(woman_id));
create policy view_visible_person on couple for select
  using (array[man_id, woman_id] && (
    select array_agg(distinct id) from app_private.visible_person_ids(false) id
  ));
create policy event_share_view on couple for select to anonymous
  using (id in (
    select couple_id from event_registration
    where registration_status = 'active' and parent_registration_id is null and couple_id is not null
      and event_id = any ((select app_private.current_shared_event_ids())::bigint[])
  ));
--! EndIncluded policies/couple.sql
drop function if exists app_private.visible_person_ids();
--! Included policies/tenant_membership.sql
select app_private.drop_policies('public.tenant_membership');

create policy system_admin_all on tenant_membership to system_admin
  using (true);
create policy admin_insert on tenant_membership for insert to administrator
  with check (tenant_id = any ((select current_admin_tenant_ids())::bigint[])
    and app_private.can_edit_person(person_id));
create policy admin_update on tenant_membership for update to administrator
  using (tenant_id = (select current_tenant_id()))
  with check (tenant_id = (select current_tenant_id()) and app_private.can_edit_person(person_id));
create policy admin_delete on tenant_membership for delete to administrator
  using (tenant_id = (select current_tenant_id()));
create policy view_visible_person on tenant_membership for select
  using (true);
--! EndIncluded policies/tenant_membership.sql
--! Included policies/tenant_trainer.sql
select app_private.drop_policies('public.tenant_trainer');

create policy system_admin_all on tenant_trainer to system_admin
  using (true);
create policy admin_insert on tenant_trainer for insert to administrator
  with check (tenant_id = any ((select current_admin_tenant_ids())::bigint[])
    and app_private.can_edit_person(person_id));
create policy admin_update on tenant_trainer for update to administrator
  using (tenant_id = (select current_tenant_id()))
  with check (tenant_id = (select current_tenant_id()) and app_private.can_edit_person(person_id));
create policy admin_delete on tenant_trainer for delete to administrator
  using (tenant_id = (select current_tenant_id()));
create policy public_view on tenant_trainer for select
  using (true);
--! EndIncluded policies/tenant_trainer.sql
--! Included policies/tenant_administrator.sql
select app_private.drop_policies('public.tenant_administrator');

create policy system_admin_all on tenant_administrator to system_admin
  using (true);
create policy admin_insert on tenant_administrator for insert to administrator
  with check (tenant_id = any ((select current_admin_tenant_ids())::bigint[])
    and app_private.can_edit_person(person_id));
create policy admin_update on tenant_administrator for update to administrator
  using (tenant_id = (select current_tenant_id()))
  with check (tenant_id = (select current_tenant_id()) and app_private.can_edit_person(person_id));
create policy admin_delete on tenant_administrator for delete to administrator
  using (tenant_id = (select current_tenant_id()));
create policy public_view on tenant_administrator for select
  using (true);
--! EndIncluded policies/tenant_administrator.sql
--! Included policies/tenant.sql
select app_private.drop_policies('public.tenant');

create policy system_admin_all on tenant to system_admin
  using (true);
create policy admin_all on tenant to administrator
  using (id = (select current_tenant_id()));
create policy admin_view on tenant for select to administrator
  using (id = any ((select current_admin_tenant_ids())::bigint[]));
create policy public_view on tenant for select to anonymous
  using (id = (select current_tenant_id()));
--! EndIncluded policies/tenant.sql

--! Included functions/login.sql
create or replace function login(login text, passwd text) returns login_result
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
declare
  v_salt text;
  usr users;
  jwt jwt_token;
begin
  v_salt := encode(digest('######TK.-.OLYMP######', 'md5'), 'hex');

  select u.* into usr
  from users u
  where (lower(u.u_login) = lower(trim(login)) or lower(u.u_email) = lower(trim(login)))
    and u.u_pass = encode(digest(v_salt || passwd || v_salt, 'sha1'), 'hex');

  if usr is null then
    insert into security_event (user_id, kind, method)
    values ((select id from users
      where lower(u_login) = lower(trim(login)) or lower(u_email) = lower(trim(login))
      limit 1), 'login_failed', 'password');
    return null;
  end if;

  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);
  update users set last_login = now() where id = usr.id;
  insert into security_event (user_id, kind, method)
  values (usr.id, 'login_succeeded', 'password');
  return (usr, jwt);
end;
$$;

grant all on function login to anonymous;
--! EndIncluded functions/login.sql
alter table otp_token drop constraint if exists otp_token_intent_check;
alter table otp_token add constraint otp_token_intent_check check (
  (intent in ('password_reset', 'tenant_handoff') and event_id is null)
  or (intent = 'event_handoff' and event_id is not null)
);

--! Included functions/otp_login.sql
drop function if exists otp_login;

create or replace function otp_login(token uuid)
  returns login_result
  language plpgsql
  strict security definer
  set search_path to pg_catalog, public, pg_temp
  as $$
declare
  v_token otp_token;
  usr users;
  jwt jwt_token;
begin
  update otp_token t set used_at = now()
  where t.access_token = token and t.used_at is null and t.expires_at > now()
    and t.user_id is not null
    and (
      t.intent = 'password_reset'
      or (t.intent in ('event_handoff', 'tenant_handoff') and t.tenant_id = current_tenant_id())
    )
  returning t.* into v_token;
  if not found then
    insert into security_event (user_id, kind, method)
    values ((select user_id from otp_token where access_token = token), 'login_failed', 'otp');
    return null;
  end if;
  select * into usr from users where id = v_token.user_id;

  jwt := app_private.create_jwt_token(usr);
  perform set_config('jwt.claims.user_id', jwt.user_id::text, true);
  perform set_config('jwt.claims.my_person_ids', jwt.my_person_ids::text, true);
  perform set_config('jwt.claims.my_tenant_ids', jwt.my_tenant_ids::text, true);
  perform set_config('jwt.claims.my_cohort_ids', jwt.my_cohort_ids::text, true);
  perform set_config('jwt.claims.my_couple_ids', jwt.my_couple_ids::text, true);

  update users set last_login = now() where id = usr.id;
  insert into security_event (user_id, kind, method)
  values (usr.id, 'login_succeeded', 'otp');
  return (usr, jwt);
end;
$$;

grant all on function otp_login to anonymous;
--! EndIncluded functions/otp_login.sql

create or replace function app_private.normalize_name(text)
returns text
language sql
immutable parallel safe strict
return lower(public.unaccent('public.unaccent'::regdictionary, $1));

--! Included functions/event_conflicts.sql
create or replace function event_conflicts(
  start_range timestamptz,
  end_range timestamptz,
  condition event_search_condition default null
) returns setof event_conflict
language sql stable as $$
  with events as materialized (
    select * from event e
    where e.tenant_id = current_tenant_id() and not e.is_cancelled
      and start_range is not null
      and app_private.tstzrange_overlaps(e.range, tstzrange(start_range, end_range, '[)'))
  ), participation as materialized (
    select e.id as event_id, p.*
    from events e
    cross join lateral (
      select person_id, true as trainer, null::bigint as couple_id
      from event_trainer where event_id = e.id
      union all
      select r.person_id, false, case when (condition).couple_id is not null then (
        select parent.couple_id from event_registration parent where parent.id = r.parent_registration_id
      ) end
      from event_registration r
      where r.event_id = e.id and r.registration_status = 'active' and r.person_id is not null
    ) p
  ), mine as (
    select event_id from event_registration
    where person_id = any((select current_person_ids())::bigint[]) and registration_status = 'active'
    union all
    select event_id from event_trainer
    where person_id = any((select current_person_ids())::bigint[])
  ), selected as (
    select e.id from events e
    where ((condition).types is null or e.type = any((condition).types))
      and ((condition).location_ids is null or e.location_id = any((condition).location_ids))
      and ((condition).parent_id is null or e.parent_id = (condition).parent_id)
      and ((condition).can_register is null or (event_registration_info(e::event)).can_register = (condition).can_register)
      and (not coalesce((condition).only_mine, false) or e.id in (select event_id from mine))
      and (not coalesce((condition).relevant_only, false) or e.parent_id is null
        or e.id in (select event_id from mine) or e.parent_id in (select event_id from mine))
      and ((condition).trainer_ids is null or exists (
        select 1 from participation p where p.event_id = e.id and p.trainer
          and p.person_id = any((condition).trainer_ids)
      ))
      and ((condition).couple_id is null or exists (
        select 1 from event_registration r where r.event_id = e.id
          and r.registration_status = 'active' and r.couple_id = (condition).couple_id
      ))
      and (((condition).participant_ids is null and (condition).cohort_id is null) or exists (
        select 1 from participation p
        where p.event_id = e.id
          and ((condition).couple_id is null or p.couple_id = (condition).couple_id)
          and ((condition).participant_ids is null or p.person_id = any((condition).participant_ids))
          and ((condition).cohort_id is null or exists (
            select 1 from current_cohort_membership c
            where c.cohort_id = (condition).cohort_id and c.person_id = p.person_id
          ))
      ))
  ), pairs as materialized (
    select a.id as first_id, a.name as first_name, a.since as first_since, a.until as first_until,
      b.id as second_id, b.name as second_name, b.since as second_since, b.until as second_until
    from events a
    join event b on a.id < b.id
      and b.tenant_id = current_tenant_id() and not b.is_cancelled
      and app_private.tstzrange_overlaps(b.range, a.range)
      and a.parent_id is distinct from b.id and b.parent_id is distinct from a.id
      and greatest(a.since, b.since) < least(a.until, b.until)
    -- Keep the counterpart even when only one event matches the calendar selectors.
    where a.id in (select id from selected) or b.id in (select id from selected)
  )
  select distinct a.person_id, p.name,
    pair.first_id, pair.first_name, pair.first_since, pair.first_until,
    pair.second_id, pair.second_name, pair.second_since, pair.second_until,
    tstzrange(greatest(pair.first_since, pair.second_since), least(pair.first_until, pair.second_until), '[)')
  from pairs pair
  join participation a on a.event_id = pair.first_id
  join participation b on b.event_id = pair.second_id and b.person_id = a.person_id
  join person p on p.id = a.person_id
  order by pair.first_since, pair.first_id, pair.second_since, pair.second_id, a.person_id;
$$;
comment on function event_conflicts is '@behavior +list -connection';
grant execute on function event_conflicts to anonymous;
--! EndIncluded functions/event_conflicts.sql

drop function if exists event_overlaps_attendee_report(timestamptz, timestamptz);
drop function if exists event_overlaps_trainer_report(timestamptz, timestamptz);

--! Included functions/payment_facets.sql
create or replace function app_private.payment_search_ids(search text)
returns setof bigint language sql stable as $$
  with term as not materialized (
    select lower(trim(search)) as value,
      (select string_agg(array_to_tsvector(array[token])::text || ':*', ' & ')::tsquery
       from unnest(tsvector_to_array(to_tsvector('pg_catalog.simple', app_private.normalize_name(search)))) token) as prefix
  )
  select p.id from payment p, term
  where strpos(p.id::text, term.value) > 0
    or strpos(lower(p.variable_symbol), term.value) > 0
    or strpos(lower(p.specific_symbol), term.value) > 0
  union all
  select p.id from payment p
  join event e on e.id = p.event_id, term
  where e.search_vector @@ term.prefix
  union all
  select p.id from payment p
  join cohort_subscription cs on cs.id = p.cohort_subscription_id
  join cohort c on c.id = cs.cohort_id, term
  where c.search_vector @@ term.prefix
  union all
  select d.payment_id from payment_debtor d
  join person p on p.id = d.person_id, term
  where p.search_vector @@ term.prefix;
$$;
grant execute on function app_private.payment_search_ids to anonymous;

create or replace function payment_facets(
  only_status payment_status default null,
  only_kind payment_kind default null,
  person_ids bigint[] default null,
  search text default null
) returns table(facet text, value text, count integer, label text)
language sql stable as $$
  with candidates as materialized (
    select p.id, p.status, p.kind,
      person_ids is null or p.id in (
        select payment_id from payment_debtor where person_id = any(person_ids)
      ) as person_matches
    from payment p
    where nullif(trim(search), '') is null
      or p.id in (select * from app_private.payment_search_ids(search))
  ), summary as (
    select p.status, p.kind, count(*) as count
    from candidates p where p.person_matches
    group by p.status, p.kind
  ), person_counts as (
    select d.person_id, count(distinct d.payment_id)::integer as count
    from payment_debtor d
    -- Keep the index order for distinct counts instead of joining and sorting debtors.
    where (d.payment_id in (
      select p.id from candidates p
      where (only_status is null or p.status = only_status)
        and (only_kind is null or p.kind = only_kind)
    )) is true
    group by d.person_id
  )
  select 'status', coalesce(upper(p.status::text), ''), coalesce(sum(p.count), 0)::integer, null::text
  from summary p
  where only_kind is null or p.kind = only_kind
  group by rollup(p.status)
  union all
  select 'kind', coalesce(upper(replace(p.kind::text, '-', '_')), ''), coalesce(sum(p.count), 0)::integer, null::text
  from summary p
  where only_status is null or p.status = only_status
  group by rollup(p.kind)
  union all
  select 'person', p.id::text, coalesce(c.count, 0), p.name
  from person p left join person_counts c on c.person_id = p.id
  where c.person_id is not null or p.id = any(person_ids);
$$;
comment on function payment_facets is '@behavior +list -connection';
grant execute on function payment_facets to anonymous;
--! EndIncluded functions/payment_facets.sql
