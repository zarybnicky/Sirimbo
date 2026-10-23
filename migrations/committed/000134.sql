--! Previous: sha1:1178896e66200153c9c92845f2d5fcc16d2a1037
--! Hash: sha1:83abb1b56c45413da3e329ed5c7e0e5924459980

--! split: 1-current.sql
comment on table person is '@behavior -insert
@ref federationPeople via:public.person_federation_link;federated.person
@ref federationCategoryProgresses via:public.person_federation_link;federated.person;federated.competitor_component;federated.competitor;federated.competitor_category_progress
@ref federationRanklistEntries via:public.person_federation_link;federated.person;federated.competitor_component;federated.competitor;federated.ranklist_entry';
comment on table couple is '@behavior +list -connection -query:resource:list -query:resource:connection
@ref federationCompetitors via:public.couple_federation_link;federated.competitor
@ref federationRanklistEntries via:public.couple_federation_link;federated.competitor;federated.ranklist_entry';

comment on table users is '@behavior -insert -update -delete';
comment on table payment is '@behavior +list -connection -insert -query:resource:list -query:resource:connection';
comment on table event is '@behavior +list -connection -insert';
comment on table tenant_trainer is '@behavior +list +connection -query:resource:list -query:resource:connection';
comment on table event_lesson_request is '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';
comment on table event_registration is '@behavior +list +connection -insert -update -delete';
comment on table access_credential is '@behavior +list -connection -delete';
comment on table account is '@behavior +list -connection -insert -update -delete -query:resource:single';
comment on table posting is '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';
comment on table transaction is '@behavior -insert -update';
comment on table announcement is '@behavior -insert';
comment on table cohort is '@behavior +list -connection';
comment on table cohort_subscription is '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';
comment on table event_trainer is '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';
comment on table tenant is '@behavior +list -connection -insert -delete -singularRelation:resource:single -query:resource:connection';
comment on table payment_debtor is '@behavior +list -connection -insert -update -delete';
comment on table membership_application is '@behavior +list -connection';
comment on table tenant_administrator is '@behavior +list +connection -query:resource:list -query:resource:connection';
comment on table tenant_membership is '@behavior +list +connection -query:resource:list -query:resource:connection';
comment on table tenant_settings is '@behavior -insert -delete -query:resource:list -query:resource:connection -singularRelation:resource:list';
comment on table aktuality is '@behavior -insert -update -query:resource:list';
comment on table location is '@behavior +list -connection -query:resource:list -query:resource:connection -queryField:resource:connection';
comment on table access_event is '@behavior +list -connection -insert -update -delete';
comment on table accounting_period is '@behavior -*';
comment on table dokumenty is '@behavior -*';
comment on table otp_token is '@behavior -*';
comment on table announcement_audience is '@behavior -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';
comment on table cohort_group is '@behavior -insert -update -delete';
comment on table cohort_membership is '@behavior +list -connection -query:resource:list -query:resource:connection';
comment on table event_target_cohort is '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';
comment on table file is '@behavior -insert -update -delete';
comment on table form_responses is '@behavior -insert -update -delete';
comment on table location_image is '@behavior +list -connection -insert -update -delete -query:resource:connection -query:resource:list';
comment on table payment_recipient is '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection';
comment on table person_invitation is '@behavior +list -connection -update -query:resource:list -query:resource:connection';
comment on table user_proxy is '@behavior +list -connection -query:resource:list -query:resource:connection';
comment on table scoreboard_manual_adjustment is '@behavior +list -connection -query:resource:list -query:resource:connection';
comment on table security_event is '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';

comment on function create_missing_cohort_subscription_payments is '@behavior -*';
comment on function create_next_cohort_subscription_payment is '@behavior -*';
comment on function current_tenant_id is '@behavior -*';
comment on function current_user_id is '@behavior -*';
comment on function verify_function is '@behavior -*';
comment on function payment_debtor_is_unpaid is '@behavior +filter +filterProc +filterBy';

comment on column access_credential.valid_range is '@behavior -*';
comment on column cohort_membership.active_range is '@behavior -*';
comment on column couple.legacy_pary_id is '@behavior -*';
comment on column couple.active_range is '@behavior -*';
comment on column event.share_token is '@behavior -*';
comment on column event_registration.created_by is '@behavior -*';
comment on column tenant_administrator.active_range is '@behavior -*';
comment on column tenant_membership.active_range is '@behavior -*';
comment on column tenant_trainer.active_range is '@behavior -*';
comment on column person_invitation.event_id is '@behavior -*';
comment on column person.legacy_user_id is '@behavior -*';
comment on column person.search_name is '@behavior -*';
comment on column user_proxy.active_range is '@behavior -*';
comment on column users.u_pass is '@behavior -*';

comment on view current_cohort_membership is '@behavior -*';
comment on view current_tenant_administrator is '@behavior -*';
comment on view current_tenant_membership is '@behavior -*';
comment on view current_tenant_trainer is '@behavior -*';
comment on materialized view account_balances is '@behavior -*';

comment on constraint event_registration_created_by_fkey on event_registration is '@behavior -*';
comment on constraint person_invitation_event_id_fkey on person_invitation is '@behavior -*';

--! Included views/competition_participation.sql
create or replace view federated.competition_participation
with (security_invoker = true) as
select competition_id, competitor_id from federated.competition_result
union all
select e.competition_id, e.competitor_id
from federated.competition_entry e
where not exists (
  select 1 from federated.competition_result r
  where (r.competition_id, r.competitor_id) = (e.competition_id, e.competitor_id)
);

comment on view federated.competition_participation is '@name competition_participation
@primaryKey competition_id,competitor_id
@foreignKey (competition_id) references federated.competition (id)|@fieldName competition|@foreignFieldName participations
@foreignKey (competitor_id) references federated.competitor (id)|@fieldName competitor|@foreignFieldName competitionParticipations
@behavior -single -list -connection -insert -update -delete';
grant select on federated.competition_participation to anonymous;
--! EndIncluded views/competition_participation.sql
--! Included functions/competition_reports.sql
create or replace function competition_brief(
  p_since date default (date_trunc('week', now())::date + 4),
  p_until date default (date_trunc('week', now())::date + 7),
  p_cohort_id bigint default null,
  p_person_ids bigint[] default null
) returns setof competition_participation_record
  language sql stable
as $$
  with scoped_people as (
    select p.id, p.name
    from person p
    where (p.id = any(p_person_ids)
       or (p_person_ids is null and exists (
         select 1 from current_tenant_membership where person_id = p.id
       )))
      and (p_cohort_id is null
       or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
  ),
  federated_people as (
    select
      sp.id as person_id,
      sp.name as person_name,
      fp.id as federated_person_id,
      fp.federation
    from scoped_people sp
    join person_federation_link link on link.person_id = sp.id
    join federated.person fp
      on (fp.federation, fp.external_id) = (link.federation, link.external_id)
  )
  select
    fp.person_id,
    fp.person_name,
    fp.federation,
    fp.federated_person_id,
    c.id as competitor_id,
    c.name as competitor_name,
    c.competitor_type,
    e.id as event_id,
    e.name as event_name,
    coalesce(e.location, e.city) as event_location,
    comp.id as competition_id,
    comp.start_date as competition_date,
    comp.check_in_end,
    cat as category,
    dances.dances,
    comp.participants_total as participants,
    null::integer as ranking,
    null::integer as ranking_to,
    null::numeric(10,3) as point_gain,
    null::boolean as is_final,
    false as has_result,
    comp.competition_type,
    e.external_id as event_external_id,
    comp.external_id as competition_external_id
  from federated_people fp
  join federated.competitor_component cc on cc.person_id = fp.federated_person_id
  join federated.competitor c on c.id = cc.competitor_id
  join federated.competition_entry ce on ce.competitor_id = c.id and not ce.cancelled
  join federated.competition comp on comp.id = ce.competition_id
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  left join lateral (
    select coalesce(array_agg(d.name order by dpd.dance_order), '{}'::text[]) as dances
    from federated.dance_program_dance dpd
    join federated.dance d on d.code = dpd.dance_code
    where dpd.program_id = cat.base_dance_program_id
  ) dances on true
  where comp.start_date >= p_since
    and comp.start_date < p_until
  order by
    comp.start_date,
    comp.check_in_end nulls last,
    fp.person_name,
    cat.discipline,
    cat.class,
    c.name;
$$;

create or replace function competition_report(
  p_since date default (date_trunc('week', now())::date - 2),
  p_until date default date_trunc('week', now())::date,
  p_cohort_id bigint default null,
  p_person_ids bigint[] default null
) returns setof competition_participation_record
  language sql stable
as $$
  with scoped_people as (
    select p.id, p.name
    from person p
    where (p.id = any(p_person_ids)
       or (p_person_ids is null and exists (
         select 1 from current_tenant_membership where person_id = p.id
       )))
      and (p_cohort_id is null
       or exists (select 1 from current_cohort_membership cm where cm.person_id = p.id and cm.cohort_id = p_cohort_id))
  ),
  federated_people as (
    select
      sp.id as person_id,
      sp.name as person_name,
      fp.id as federated_person_id,
      fp.federation
    from scoped_people sp
    join person_federation_link link on link.person_id = sp.id
    join federated.person fp
      on (fp.federation, fp.external_id) = (link.federation, link.external_id)
  )
  select
    fp.person_id,
    fp.person_name,
    fp.federation,
    fp.federated_person_id,
    c.id as competitor_id,
    c.name as competitor_name,
    c.competitor_type,
    e.id as event_id,
    e.name as event_name,
    coalesce(e.location, e.city) as event_location,
    comp.id as competition_id,
    comp.start_date as competition_date,
    comp.check_in_end,
    cat as category,
    coalesce(dances.dances, '{}'::text[]) as dances,
    comp.participants_total as participants,
    cr.ranking,
    cr.ranking_to,
    cr.point_gain,
    cr.is_final,
    true as has_result,
    comp.competition_type,
    e.external_id as event_external_id,
    comp.external_id as competition_external_id
  from federated_people fp
  join federated.competitor_component cc on cc.person_id = fp.federated_person_id
  join federated.competitor c on c.id = cc.competitor_id
  join federated.competition_result cr on cr.competitor_id = c.id
  join federated.competition comp on comp.id = cr.competition_id
  join federated.event e on e.id = comp.event_id
  join federated.category cat on cat.id = comp.category_id
  left join lateral (
    select array_agg(d.name order by rd.dance_order) as dances
    from (
      select r.id
      from federated.competition_round r
      where r.competition_id = comp.id
      order by r.round_index desc, (r.round_key = 'F') desc, r.id desc
      limit 1
    ) r
    join federated.round_dance rd on rd.round_id = r.id
    join federated.dance d on d.code = rd.dance_code
  ) dances on true
  where comp.start_date >= p_since
    and comp.start_date < p_until
  order by
    comp.start_date,
    fp.person_name,
    cat.discipline,
    cat.class,
    cr.ranking,
    c.name;
$$;

comment on function competition_brief is '@behavior -*';
grant all on function competition_brief to anonymous;

comment on function competition_report is '@behavior -*';
grant all on function competition_report to anonymous;
--! EndIncluded functions/competition_reports.sql
--! Included functions/create_event_payment.sql
drop function if exists create_event_instance_payment;

create or replace function create_event_payment(target_event event)
  returns payment
  language plpgsql
as $$
declare
  created_payment payment;
  duration numeric(19, 4);
begin
  if current_tenant_id() <> 2 then
    return null;
  end if;

  -- Serialize initial charging with registration changes and their deferred sync.
  select * into target_event from event
  where id = target_event.id and tenant_id = current_tenant_id() for no key update;
  if not found or target_event.is_cancelled then return null; end if;

  select p.* into created_payment from payment p where p.event_id = target_event.id;

  if found then
    return created_payment;
  end if;

  if target_event.type <> 'lesson' or not exists (
    select 1
    from event_registration registration
    where registration.event_id = target_event.id
      and registration.person_id is not null
      and registration.registration_status = 'active'
  ) then
    return null;
  end if;

  duration := extract(epoch from (target_event.until - target_event.since)) / 60;

  insert into payment (accounting_period_id, status, event_id, due_at)
  values (
    (select id from accounting_period where tenant_id = current_tenant_id() and range @> now()),
    'tentative', target_event.id, now() + interval '2 weeks'
  )
  on conflict (event_id) where event_id is not null do nothing
  returning * into created_payment;

  if not found then
    return (select p from payment p where p.event_id = target_event.id);
  end if;

  insert into payment_recipient (payment_id, account_id, amount)
  select distinct on (account.id)
    created_payment.id,
    account.id,
    trainer.member_price_45min_amount / 45 * duration
  from event_trainers(target_event) trainer
  cross join lateral person_account(trainer.person_id, trainer.currency) account
  where trainer.tenant_id = current_tenant_id()
    and trainer.member_price_45min_amount is not null;

  insert into payment_debtor (payment_id, person_id)
  select distinct created_payment.id, registration.person_id
  from event_registration registration
  where registration.event_id = target_event.id
    and registration.person_id is not null
    and registration.registration_status = 'active';

  return created_payment;
end
$$;

COMMENT ON FUNCTION create_event_payment IS '@behavior -*';
GRANT ALL ON FUNCTION create_event_payment TO anonymous;
--! EndIncluded functions/create_event_payment.sql
--! Included functions/current_account_ids.sql
create or replace function current_account_ids() returns setof bigint
  language sql stable
as $$
  select id from account
  where person_id = any ((select current_person_ids())::bigint[]);
$$;

comment on function current_account_ids() is '@behavior -*';
grant execute on function current_account_ids() to anonymous;
--! EndIncluded functions/current_account_ids.sql
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

CREATE or replace FUNCTION my_tenants_array() RETURNS bigint[] LANGUAGE sql STABLE LEAKPROOF PARALLEL SAFE AS $$
  SELECT translate(coalesce(nullif(current_setting('jwt.claims.my_tenant_ids', true), ''), '{}'), '[]', '{}')::bigint[];
$$;
COMMENT ON FUNCTION my_tenants_array() IS '@behavior -*';
GRANT ALL ON FUNCTION my_tenants_array() TO anonymous;
--! EndIncluded functions/current_person_ids.sql
--! Included functions/event_approx_price.sql
drop function if exists event_instance_approx_price;

create or replace function event_approx_price(v_instance event)
  returns table (amount numeric(19,4), currency text)
  language sql stable
as $$
  with stats as materialized (
    select
      count(*) as num_participants,
      extract(epoch from (v_instance.until - v_instance.since)) / 60.0 as duration
    from event_registration registration
    where
      v_instance.type = 'lesson'
      and registration.event_id = v_instance.id
      and registration.person_id is not null
      and registration.registration_status = 'active'
  )
  select
    sum(tt.member_price_45min_amount * s.duration / 45 / s.num_participants) as amount,
    tt.currency as currency
  from stats s
  join lateral event_trainers(v_instance) tt on true
  where
    s.num_participants > 0
    and s.duration > 0
    and tt.member_price_45min_amount is not null
    and tt.currency is not null
  group by tt.currency;
$$;

grant all on function event_approx_price to anonymous;
comment on function event_approx_price is '@behavior +list -connection';
--! EndIncluded functions/event_approx_price.sql
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
--! Included functions/event_instances_for_range.sql
do $$
begin
  create type event_instance_range_scope as enum ('all', 'top_level', 'mine', 'relevant');
exception when duplicate_object then null;
end
$$;

drop function if exists event_instances_for_range;

create or replace function event_instances_for_range(
  only_type event_type,
  start_range timestamptz,
  end_range timestamptz = null,
  trainer_ids bigint[] = null,
  participant_ids bigint[] = null,
  parent_id bigint = null,
  scope event_instance_range_scope = 'all',
  location_ids bigint[] = null,
  only_types event_type[] = null
) returns setof event as $$
  with mine as (
    select event_id from event_registration
    where person_id = any ((select current_person_ids())::bigint[]) and registration_status = 'active'
    union all
    select event_id from event_trainer
    where person_id = any ((select current_person_ids())::bigint[])
  )
  select i.*
  from event i
  where i.tenant_id = current_tenant_id()
    and (only_type is null or i.type = only_type)
    and (only_types is null or i.type = any(only_types))
    and case
      when $6 is not null then i.parent_id = $6
        and (scope <> 'mine' or i.id in (select event_id from mine))
      when scope = 'all' then true
      when scope = 'top_level' then i.parent_id is null
      when scope = 'mine' then i.id in (select event_id from mine)
      when scope = 'relevant' then i.parent_id is null
        or i.id in (select event_id from mine)
        or i.parent_id in (select event_id from mine)
    end
    and start_range is not null
    and i.since < coalesce(end_range, 'infinity'::timestamptz)
    and app_private.tstzrange_overlaps(i.range, tstzrange(
      start_range, greatest(start_range, coalesce(end_range, 'infinity'::timestamptz)), '[]'))
    and (trainer_ids is null
      or exists (select 1 from event_trainer where event_id = i.id and person_id = any (trainer_ids)))
    and (participant_ids is null
      or exists (select 1 from event_registration where event_id = i.id and person_id = any (participant_ids) and registration_status = 'active'))
    and (location_ids is null or i.location_id = any (location_ids))
  order by i.since
  ;
$$ stable language sql;

comment on function event_instances_for_range is '@behavior +list -connection';
grant all on function event_instances_for_range to anonymous;
--! EndIncluded functions/event_instances_for_range.sql
--! Included functions/event_overlaps_reports.sql
drop function if exists event_overlaps_attendee_report;
drop function if exists event_overlaps_trainer_report;
drop type if exists event_overlaps_conflict;
drop type if exists event_conflict;

create type event_conflict as (
  person_id bigint,
  person_name text,
  first_event_id bigint,
  first_event_name text,
  first_since timestamptz,
  first_until timestamptz,
  second_event_id bigint,
  second_event_name text,
  second_since timestamptz,
  second_until timestamptz,
  overlap_range tstzrange
);

comment on type event_conflict is E'
@foreignKey (person_id) references person (id)
@foreignKey (first_event_id) references event (id)|@fieldName firstEvent
@foreignKey (second_event_id) references event (id)|@fieldName secondEvent
';
comment on column event_conflict.person_id is '@notNull';
comment on column event_conflict.first_event_id is '@notNull';
comment on column event_conflict.second_event_id is '@notNull';
comment on column event_conflict.overlap_range is '@notNull';

create or replace function event_overlaps_attendee_report(p_since timestamptz, p_until timestamptz)
  returns setof event_conflict
  language sql stable
as $$
  with instances as (
    select
      ea.person_id,
      p.name as person_name,
      ei.id as event_id,
      ei.parent_id,
      ei.since,
      ei.until,
      ei.range,
      ei.name as event_name
    from event_registration ea
    join event ei on ei.id = ea.event_id
    join person p on p.id = ea.person_id
    where not ei.is_cancelled
      and ea.person_id is not null
      and ea.registration_status = 'active'
      and app_private.tstzrange_overlaps(ei.range, tstzrange(coalesce(p_since, '-infinity'::timestamptz), coalesce(p_until, 'infinity'::timestamptz), '[]'))
  )
  select
    i1.person_id,
    i1.person_name,
    i1.event_id as first_event_id,
    i1.event_name as first_event_name,
    i1.since as first_since,
    i1.until as first_until,
    i2.event_id as second_event_id,
    i2.event_name as second_event_name,
    i2.since as second_since,
    i2.until as second_until,
    tstzrange(greatest(i1.since, i2.since), least(i1.until, i2.until), '[]') as overlap_range
  from instances i1
  join instances i2 on i1.person_id = i2.person_id
    and i1.event_id < i2.event_id
    and i1.parent_id is distinct from i2.event_id
    and i2.parent_id is distinct from i1.event_id
    and app_private.tstzrange_overlaps(i1.range, i2.range)
    and greatest(i1.since, i2.since) < least(i1.until, i2.until);
$$;

comment on function event_overlaps_attendee_report is '@behavior +list -connection';
grant all on function event_overlaps_attendee_report to anonymous;

create or replace function event_overlaps_trainer_report(p_since timestamptz, p_until timestamptz)
  returns setof event_conflict
  language sql stable
as $$
  with trainer_instances as (
    select
      eit.person_id,
      p.name as person_name,
      ei.id as event_id,
      ei.parent_id,
      ei.since,
      ei.until,
      ei.range,
      ei.name as event_name
    from event ei
    join event_trainer eit on ei.id = eit.event_id
    join person p on p.id = eit.person_id
    where not ei.is_cancelled
      and app_private.tstzrange_overlaps(ei.range, tstzrange(coalesce(p_since, '-infinity'::timestamptz), coalesce(p_until, 'infinity'::timestamptz), '[]'))
  )
  select
    ti1.person_id,
    ti1.person_name,
    ti1.event_id as first_event_id,
    ti1.event_name as first_event_name,
    ti1.since as first_since,
    ti1.until as first_until,
    ti2.event_id as second_event_id,
    ti2.event_name as second_event_name,
    ti2.since as second_since,
    ti2.until as second_until,
    tstzrange(greatest(ti1.since, ti2.since), least(ti1.until, ti2.until), '[]') as overlap_range
  from trainer_instances ti1
  join trainer_instances ti2 on ti1.person_id = ti2.person_id
    and ti1.event_id < ti2.event_id
    and ti1.parent_id is distinct from ti2.event_id
    and ti2.parent_id is distinct from ti1.event_id
    and app_private.tstzrange_overlaps(ti1.range, ti2.range)
    and greatest(ti1.since, ti2.since) < least(ti1.until, ti2.until);
$$;

comment on function event_overlaps_trainer_report is '@behavior +list -connection';
grant all on function event_overlaps_trainer_report to anonymous;
--! EndIncluded functions/event_overlaps_reports.sql
--! Included functions/event_series_info.sql
drop function if exists event_instance_series_info;

create or replace function event_series_info(instance event)
  returns series_info
  language sql
  stable
as $$
  select row(
    ranked.series_id,
    series.name,
    ranked.position,
    ranked.length,
    ranked.since,
    ranked.until
  )::series_info
  from (
    select
      member.id as member_id,
      member.series_id,
      member.tenant_id,
      row_number() over (order by member.since, member.id)::integer as position,
      count(*) over ()::integer as length,
      min(member.since) over () as since,
      max(member.until) over () as until
    from event member
    where member.series_id = instance.series_id
  ) ranked
  join event_series series on series.tenant_id = ranked.tenant_id and series.id = ranked.series_id
  where ranked.member_id = instance.id;
$$;

grant execute on function event_series_info to anonymous;
--! EndIncluded functions/event_series_info.sql
--! Included functions/event_trainers.sql
drop function if exists event_instance_trainers;
drop function if exists app_private.event_instance_trainers_at;

create or replace function app_private.event_trainers_at(
  v_instance event,
  v_at timestamptz
)
  returns setof tenant_trainer
  language sql stable
as $$
select distinct on (tt.tenant_id, tt.person_id) tt.*
from event_trainer k
join tenant_trainer tt on tt.tenant_id = k.tenant_id and tt.person_id = k.person_id
where k.event_id = v_instance.id
  and tt.active_range @> v_at
order by tt.tenant_id, tt.person_id, lower(tt.active_range) desc;
$$;
grant all on function app_private.event_trainers_at to anonymous;

create or replace function event_trainers(v_instance event)
  returns setof tenant_trainer
  language sql stable
as $$
select * from app_private.event_trainers_at(v_instance, v_instance.since);
$$;

grant all on function event_trainers to anonymous;
comment on function event_trainers is '@behavior +list -connection
@deprecated Use eventTrainersList.';
--! EndIncluded functions/event_trainers.sql
--! Included functions/get_current_user.sql
create or replace function get_current_user(version_id text default null) returns users
language sql volatile security definer
set search_path = pg_catalog, public, pg_temp
as $$
  with updated_user as (
    update users
    set
      last_active_at = now(),
      last_version = version_id
    where id = current_user_id()
    returning *
  )
  select * from updated_user
  union all
  select *
  from users
  where id = current_user_id()
    and not exists (select 1 from updated_user);
$$;

grant all on function get_current_user to anonymous;

comment on function get_current_user is '@behavior -*';
--! EndIncluded functions/get_current_user.sql
--! Included functions/payment_debtor_price.sql
drop function if exists payment_debtor_price;

create or replace function payment_debtor_price(p payment_debtor, out amount numeric(19,4), out currency text)
  language sql stable security definer
  set search_path = pg_catalog, public, pg_temp
as $$
select
  sum(payment_recipient.amount) / (
    select count(*) as count
    from payment_debtor
    where p.payment_id = payment_debtor.payment_id
  )::numeric(19,4) as amount,
  min(account.currency)::text as currency
from payment_recipient
  join account on payment_recipient.account_id = account.id
where payment_recipient.payment_id = p.payment_id
  -- Calculate the complete bill only for an authorized stored debtor.
  and exists (
    select from payment_debtor d
    where d.id = p.id and d.payment_id = p.payment_id
      and d.tenant_id = (select current_tenant_id())
      and (
        (select pg_has_role(coalesce(nullif(current_setting('role'), 'none'), session_user), 'administrator', 'member'))
        or (
          (select pg_has_role(coalesce(nullif(current_setting('role'), 'none'), session_user), 'member', 'member'))
          and d.person_id = any ((select current_person_ids())::bigint[])
        )
      )
  );
$$;

comment on function payment_debtor_price is '@behavior +list -connection';
grant all on function payment_debtor_price to anonymous;
--! EndIncluded functions/payment_debtor_price.sql
--! Included functions/person_account.sql
CREATE or replace FUNCTION person_account(p_id bigint, in_currency text, OUT acc account) RETURNS account AS $$
  WITH ins AS (
    INSERT INTO account (tenant_id, person_id, currency)
      VALUES ((select current_tenant_id()), p_id, COALESCE(in_currency, 'CZK'))
      ON CONFLICT ON CONSTRAINT account_tenant_id_person_id_currency_idx DO NOTHING
      RETURNING *
  )
  SELECT * FROM ins
  UNION ALL
  SELECT account.* FROM account
  WHERE tenant_id = (select current_tenant_id()) AND person_id = p_id AND currency = COALESCE(in_currency, 'CZK')
  LIMIT 1;
$$ language sql security definer set search_path = pg_catalog, public, pg_temp;

grant all on function person_account to anonymous;

comment on function person_account is '@behavior -*';
--! EndIncluded functions/person_account.sql
--! Included functions/person_couples.sql
CREATE or replace FUNCTION person_active_couples(p person) RETURNS SETOF couple as $$
  (select * from couple where man_id = p.id and status = 'active')
  union all
  (select * from couple where woman_id = p.id and status = 'active')
$$ language sql stable;

CREATE or replace FUNCTION person_all_couples(p person) RETURNS SETOF couple as $$
  (select * from couple where man_id = p.id)
  union all
  (select * from couple where woman_id = p.id)
$$ language sql stable;

grant all on function person_active_couples to anonymous;
grant all on function person_all_couples to anonymous;

comment on function person_active_couples is '@behavior +list -connection';

comment on function person_all_couples is '@behavior +list -connection';
--! EndIncluded functions/person_couples.sql
--! Included functions/person_federation_candidates.sql
drop function if exists person_federation_candidates;
drop function if exists federation_person_candidates;
drop type if exists person_federation_candidates_record;

create type person_federation_candidates_record as (
  id integer,
  name text,
  age_group text,
  similarity real,
  birth_year integer,
  csts_id integer,
  wdsf_id integer,
  is_strong_match boolean
);

comment on column person_federation_candidates_record.id is '@notNull';
comment on column person_federation_candidates_record.similarity is '@notNull';
comment on column person_federation_candidates_record.is_strong_match is '@notNull';

create function federation_person_candidates(
  first_name text, last_name text, federation text,
  gender gender_type default null, birth_date date default null,
  "limit" integer default 10, threshold real default 0.4
) returns setof person_federation_candidates_record
language sql stable as $$
  with input as materialized (
    select app_private.normalize_name(concat_ws(' ', trim($1), trim($2))) as name,
      app_private.normalize_name(trim($1)) as first_name,
      app_private.normalize_name(trim($2)) as last_name,
      extract(year from $5)::integer as birth_year,
      extract(year from current_date)::integer - extract(year from $5)::integer as age,
      case $4 when 'man' then 'male'::federated.gender
        when 'woman' then 'female'::federated.gender end as gender
  ), matches as (
    select fp.*, similarity(fp.search_name, i.name) as name_score,
      nullif(fp.gender, 'unknown') = i.gender as gender_match,
      coalesce(fp.birth_year, extract(year from fp.dob)::integer) = i.birth_year
        or (fp.birth_year is null and fp.dob is null
          and coalesce(g.age, g.person_age) @> i.age) as age_match
    from federated.person fp
    cross join input i
    left join federated.age_group g on g.code = fp.age_group_code
    where fp.federation = $3
      and nullif(i.first_name, '') is not null and nullif(i.last_name, '') is not null
      and fp.external_id between 1 and 2147483647
      and fp.search_name % i.name
      and similarity(fp.search_name, i.name)
        >= greatest(0.3::real, least(coalesce(threshold, 0.4), 1::real))
      -- Indexed word matching uses pg_trgm's default 0.6 threshold for each name part.
      and fp.search_name %> i.first_name
      and fp.search_name %> i.last_name
      and (i.gender is null or fp.gender is null or fp.gender = 'unknown' or fp.gender = i.gender)
      and (
        i.birth_year is null
        or coalesce(fp.birth_year, extract(year from fp.dob)::integer) = i.birth_year
        or (fp.birth_year is null and fp.dob is null and (
          coalesce(g.age, g.person_age) is null
          or coalesce(g.age, g.person_age) @> i.age
          -- Adult is also a broad label for people eligible for senior categories.
          or (g.code = 'Adult' and i.age >= lower(g.age))
        ))
      )
  ), candidates as (
    select * from matches
    order by gender_match desc nulls last, age_match desc nulls last,
      name_score desc, canonical_name, external_id
    limit greatest(0, least(coalesce($6, 10), 50))
  )
  select fp.external_id::integer, fp.canonical_name, fp.age_group_code, fp.name_score,
    coalesce(fp.birth_year, extract(year from fp.dob)::integer),
    case fp.federation when 'csts' then fp.external_id::integer else linked.external_id end,
    case fp.federation when 'wdsf' then fp.external_id::integer else linked.external_id end,
    fp.name_score = 1 and fp.gender_match is true and fp.age_match is true
      and not exists (
        select 1 from matches other
        where other.external_id <> fp.external_id and other.name_score = 1
      )
  from candidates fp
  left join lateral (
    select min(other.external_id)::integer as external_id
    from federated.person other
    where (
      (fp.federation = 'wdsf' and other.federation = 'csts'
        and other.external_id::text = fp.national_reference)
      or (fp.federation = 'csts' and other.federation = 'wdsf'
        and other.national_reference = fp.external_id::text)
    )
      and other.external_id between 1 and 2147483647
      and similarity(other.search_name, fp.search_name) >= 0.8
      and (other.gender is null or other.gender = 'unknown'
        or fp.gender is null or fp.gender = 'unknown' or other.gender = fp.gender)
      and (coalesce(other.birth_year, extract(year from other.dob)::integer) is null
        or coalesce(fp.birth_year, extract(year from fp.dob)::integer) is null
        or coalesce(other.birth_year, extract(year from other.dob)::integer)
          = coalesce(fp.birth_year, extract(year from fp.dob)::integer))
    having count(*) = 1
  ) linked on true
  order by fp.gender_match desc nulls last, fp.age_match desc nulls last,
    fp.name_score desc, fp.canonical_name, fp.external_id;
$$;

comment on function federation_person_candidates is '@behavior +list -connection
@notNull';
grant execute on function federation_person_candidates to anonymous;

create function person_federation_candidates(
  in_person person, federation text, "limit" integer default 10, threshold real default 0.4
) returns setof person_federation_candidates_record
language sql stable as $$
  select * from federation_person_candidates(
    in_person.first_name, in_person.last_name, $2, in_person.gender, in_person.birth_date, $3, $4
  );
$$;

comment on function person_federation_candidates is '@behavior +list -connection
@notNull';
grant execute on function person_federation_candidates to anonymous;
--! EndIncluded functions/person_federation_candidates.sql
--! Included functions/person_with_invitation.sql
create or replace function people_without_access_with_existing_account() returns setof person stable language sql as $$
select * from person
where not exists (select 1 from user_proxy where person_id = person.id)
  and not exists (select 1 from person_invitation where person_id = person.id)
  and exists (select 1 from users where users.u_email = person.email)
  and (exists (select 1 from current_tenant_membership where person_id = person.id)
  or exists (select 1 from current_tenant_trainer where person_id = person.id)
  or exists (select 1 from current_tenant_administrator where person_id = person.id));
$$;
grant all on function people_without_access_with_existing_account to anonymous;

COMMENT ON function people_without_access_with_existing_account IS '@behavior +list -connection';

create or replace function people_without_access_or_invitation() returns setof person stable language sql as $$
select * from person
where not exists (select 1 from user_proxy where person_id = person.id)
  and not exists (select 1 from person_invitation where person_id = person.id)
  and not exists (select 1 from person_invitation where email = person.email)
  and not exists (select 1 from users where users.u_email = person.email)
  and (exists (select 1 from current_tenant_membership where person_id = person.id)
    or exists (select 1 from current_tenant_trainer where person_id = person.id)
    or exists (select 1 from current_tenant_administrator where person_id = person.id));
$$;
grant all on function people_without_access_or_invitation to anonymous;

COMMENT ON function people_without_access_or_invitation IS '@behavior +list -connection';

create or replace function people_without_access_with_invitation() returns setof person stable language sql as $$
select * from person
where not exists (select 1 from user_proxy where person_id = person.id)
  and exists (select 1 from person_invitation where person_id = person.id)
  and (exists (select 1 from current_tenant_membership where person_id = person.id)
  or exists (select 1 from current_tenant_trainer where person_id = person.id)
  or exists (select 1 from current_tenant_administrator where person_id = person.id));
$$;
grant all on function people_without_access_with_invitation to anonymous;

COMMENT ON function people_without_access_with_invitation IS '@behavior +list -connection';
--! EndIncluded functions/person_with_invitation.sql
--! Included functions/resolve_payment_with_credit.sql
CREATE or replace FUNCTION resolve_payment_with_credit(p payment) RETURNS payment
  LANGUAGE plpgsql
AS $$
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
--! EndIncluded functions/resolve_payment_with_credit.sql
--! Included functions/scoreboard.sql
drop view if exists scoreboard;
drop function if exists scoreboard_entries;

create or replace function scoreboard_entries(
  since date,
  until date,
  cohort_id bigint default null
)
returns setof scoreboard_record
language sql
stable
as $$
  with membership as (
    select cm.person_id, cm.cohort_id
    from current_cohort_membership cm
    where (cm.cohort_id = scoreboard_entries.cohort_id or scoreboard_entries.cohort_id is null)
  ),
  instances as (
    select
      i.id as event_id,
      i.since,
      i.until,
      i.type as event_type
    from event i
    where not i.is_cancelled
      and i.since >= scoreboard_entries.since::timestamptz
      and i.until  < scoreboard_entries.until::timestamptz
      and i.type <> 'reservation'
  ),
  member_people as (
    select distinct person_id
    from membership
  ),
  attendance as materialized (
    select
      ea.person_id,
      scoreboard_entries.cohort_id as cohort_id,
      case when inst.event_type = 'lesson' then 1 else 0 end as lesson_score,
      case when inst.event_type = 'group' then floor((extract(epoch from (inst.until - inst.since)) / 60)::numeric / 45::numeric) else 0 end as group_score,
      case when inst.event_type = 'camp'  then 3 + 2 * ((extract(epoch from (inst.until - inst.since)) > 86400)::int) else 0 end as event_score,
      date_trunc('day', inst.since)::date as day
    from instances inst
    join event_registration ea
      on ea.event_id = inst.event_id and ea.person_id is not null
    left join event_registration registration
      on registration.id = ea.parent_registration_id
    left join cohort tc
      on tc.id = coalesce(ea.target_cohort_id, registration.target_cohort_id)
    where
      ea.registration_status = 'active'
      and (ea.status = 'attended' or inst.event_type = 'lesson')
      and ea.person_id = any (select person_id from member_people)
      and (scoreboard_entries.cohort_id is null or tc.id = scoreboard_entries.cohort_id)
  ),
  per_day as (
    select
      person_id,
      cohort_id,
      least(sum(lesson_score), 4) as lesson_score,
      sum(group_score) as group_score,
      sum(event_score) as event_score
    from attendance
    group by person_id, cohort_id, day
  ),
  aggregated as (
    select
      person_id,
      cohort_id,
      coalesce(sum(lesson_score), 0)::bigint as lesson_total_score,
      coalesce(sum(group_score), 0)::bigint as group_total_score,
      coalesce(sum(event_score), 0)::bigint as event_total_score
    from per_day
    group by person_id, cohort_id
  ),
  manual as (
    select
      sma.person_id,
      case
        when scoreboard_entries.cohort_id is null then null
        else coalesce(sma.cohort_id, scoreboard_entries.cohort_id)
      end as cohort_id,
      sum(sma.points)::bigint as manual_total_score
    from scoreboard_manual_adjustment sma
    where sma.tenant_id = current_tenant_id()
      and sma.awarded_at >= scoreboard_entries.since
      and sma.awarded_at < scoreboard_entries.until
      and (
        scoreboard_entries.cohort_id is null or
        sma.cohort_id is null or
        sma.cohort_id = scoreboard_entries.cohort_id
      )
      and exists (
        select 1
        from membership mem
        where mem.person_id = sma.person_id
          and (
            (scoreboard_entries.cohort_id is null and sma.cohort_id is null)
            or mem.cohort_id is not distinct from coalesce(sma.cohort_id, scoreboard_entries.cohort_id)
          )
      )
    group by 1, 2
  ),
  totals as (
    select
      coalesce(a.person_id, m.person_id) as person_id,
      coalesce(a.cohort_id, m.cohort_id) as cohort_id,
      coalesce(a.lesson_total_score, 0)::bigint as lesson_total_score,
      coalesce(a.group_total_score, 0)::bigint as group_total_score,
      coalesce(a.event_total_score, 0)::bigint as event_total_score,
      coalesce(m.manual_total_score, 0)::bigint as manual_total_score,
      (
        coalesce(a.lesson_total_score, 0)::bigint +
        coalesce(a.group_total_score, 0)::bigint +
        coalesce(a.event_total_score, 0)::bigint +
        coalesce(m.manual_total_score, 0)::bigint
      ) as total_score
    from aggregated a
    full join manual m using (person_id, cohort_id)
  )
  select
    person_id,
    cohort_id,
    lesson_total_score,
    group_total_score,
    event_total_score,
    manual_total_score,
    total_score,
    rank() over (order by total_score desc, person_id) as ranking
  from totals
  order by total_score desc, person_id;
$$;

comment on function scoreboard_entries is '@behavior +list -connection';

grant all on function scoreboard_entries to anonymous;
--! EndIncluded functions/scoreboard.sql
--! Included functions/tenant_account.sql
CREATE or replace FUNCTION tenant_account(in_currency text, OUT acc account) RETURNS account
  language sql security definer
  SET search_path TO pg_catalog, public, pg_temp
  AS $$
  WITH ins AS (
    INSERT INTO account (tenant_id, person_id, currency)
      VALUES ((select current_tenant_id()), NULL::bigint, COALESCE(in_currency, 'CZK'))
      ON CONFLICT ON CONSTRAINT account_tenant_id_person_id_currency_idx DO NOTHING
      RETURNING *
  )
  SELECT * FROM ins
  UNION ALL
  SELECT account.* FROM account
    WHERE tenant_id = (select current_tenant_id()) AND person_id IS NULL AND currency = COALESCE(in_currency, 'CZK')
  LIMIT 1;
$$;

grant all on function tenant_account to anonymous;

comment on function tenant_account is '@behavior -*';
--! EndIncluded functions/tenant_account.sql
--! Included functions/tenant_couples.sql
create or replace function tenant_couples(t tenant) returns setof couple as $$
  select couple.*
  from couple
  where couple.status = 'active'
    and (exists (select 1 from current_tenant_membership where person_id = man_id)
      or exists (select 1 from current_tenant_membership where person_id = woman_id));
$$ language sql stable;
grant all on function tenant_couples to anonymous;
comment on function tenant_couples is '@behavior +list -connection';
--! EndIncluded functions/tenant_couples.sql
--! Included functions/trainer_group_attendance_completion.sql
drop function if exists trainer_group_attendance_completion;
drop type if exists trainer_group_attendance_completion;

create type trainer_group_attendance_completion as (
  person_id bigint,
  total_events integer,
  filled_events integer,
  partially_filled_events integer,
  unfilled_events integer,
  filled_ratio double precision,
  total_attendances integer,
  pending_attendances integer
);

comment on type trainer_group_attendance_completion is '@foreignKey (person_id) references person (id)';

create or replace function trainer_group_attendance_completion(
  since timestamp with time zone default null,
  until timestamp with time zone default null
) returns setof trainer_group_attendance_completion
  language sql stable as $_$
  with filtered_instances as (
    select ei.id
    from event ei
    where ei.tenant_id = current_tenant_id()
      and not ei.is_cancelled
      and ei.type = 'group'
      and coalesce(ei.until, ei.since) < coalesce($2, now())
      and ($1 is null or coalesce(ei.since, ei.until) >= $1)
      and ($2 is null or coalesce(ei.until, ei.since) < $2)
  ),
  trainer_instances as (
    select effective_trainer.person_id, fi.id as event_id
    from filtered_instances fi
    join event instance on instance.id = fi.id
    cross join lateral app_private.event_trainers_at(instance, instance.since) effective_trainer
  ),
  attendance_stats as (
    select
      ti.person_id,
      ti.event_id,
      coalesce(stats.attendance_count, 0) as attendance_count,
      coalesce(stats.unknown_count, 0) as unknown_count
    from trainer_instances ti
    left join lateral (
      select
        count(*) as attendance_count,
        count(*) filter (where eir.status = 'unknown') as unknown_count
      from event_registration eir
      where eir.event_id = ti.event_id
        and eir.person_id is not null
        and eir.registration_status = 'active'
    ) stats on true
  ),
  per_trainer as (
    select
      person_id,
      count(*) as total_events,
      count(*) filter (where attendance_count > 0 and unknown_count = 0) as filled_events,
      count(*) filter (
        where attendance_count > 0
          and unknown_count > 0
          and unknown_count < attendance_count
      ) as partially_filled_events,
      count(*) filter (where attendance_count = 0 or unknown_count = attendance_count) as unfilled_events,
      coalesce(sum(attendance_count), 0) as total_attendances,
      coalesce(sum(unknown_count), 0) as pending_attendances
    from attendance_stats
    group by person_id
  )
  select
    person_id,
    total_events,
    filled_events,
    partially_filled_events,
    unfilled_events,
    case
      when total_events > 0 then (filled_events + partially_filled_events)::double precision / total_events
      else null
    end as filled_ratio,
    total_attendances,
    pending_attendances
  from per_trainer
  order by filled_ratio asc nulls last, person_id;
$_$;

comment on function trainer_group_attendance_completion is '@behavior +list -connection';
grant all on function trainer_group_attendance_completion to anonymous;
--! EndIncluded functions/trainer_group_attendance_completion.sql
--! Included functions/log_in_as.sql
drop function if exists log_in_as;

CREATE FUNCTION log_in_as(id bigint) RETURNS login_result
  LANGUAGE plpgsql
  STRICT SECURITY DEFINER
  SET search_path TO pg_catalog, public, pg_temp
AS $$
declare
  v_id alias for $1;
  usr users;
  jwt jwt_token;
begin
  select * into strict usr from users where users.id = v_id;
  jwt := app_private.create_jwt_token(usr);
  insert into security_event (user_id, kind, method)
  values (usr.id, 'impersonation', 'manual');
  return (usr, jwt);
end;
$$;

revoke all on FUNCTION log_in_as from public, anonymous;
GRANT ALL ON FUNCTION log_in_as TO administrator;
--! EndIncluded functions/log_in_as.sql
--! Included functions/refresh_jwt.sql
create or replace function refresh_jwt() returns jwt_token
  language sql
  stable
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select app_private.create_jwt_token(users)
  from users
  where id = current_user_id();
$$;

grant all on function refresh_jwt to anonymous;
--! EndIncluded functions/refresh_jwt.sql
--! Included functions/current_claims.sql
create or replace function current_claims() returns jsonb
  language sql stable
  security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select to_jsonb(app_private.create_jwt_token(users)) - 'exp'
  from users
  where id = nullif(current_setting('jwt.claims.user_id', true), '')::bigint;
$$;

grant all on function current_claims to anonymous;
--! EndIncluded functions/current_claims.sql
--! Included functions/submit_form.sql
CREATE or replace FUNCTION submit_form(type text, data jsonb, url text) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    SET search_path TO pg_catalog, public, pg_temp
    AS $$
declare
  v_email text;
begin
  insert into form_responses (type, data, url) values (type, data, url);

  if current_tenant_id() = 1 then
    foreach v_email in array (array['m.hyzova96@seznam.cz', 'miroslav.hyza@tkolymp.cz', 'hyzam@tkolymp.cz', 'filip.karasek@tkolymp.cz']) loop
      perform graphile_worker.add_job(
        'send_email',
        json_build_object(
          'template', 'notify_submitted_form.mjml',
          'options', json_build_object(
          'to', v_email,
          'subject', 'Nový vyplněný formulář z webu'
        ),
        'variables', json_build_object(
          'url', url,
          'data', data
        )
      ));
    end loop;
  end if;
end;
$$;

GRANT ALL ON FUNCTION submit_form TO anonymous;
--! EndIncluded functions/submit_form.sql
--! Included functions/tg_cohort_membership__on_status.sql
CREATE or replace FUNCTION app_private.tg_cohort_membership__on_status()
  RETURNS trigger LANGUAGE plpgsql
  security definer
  SET search_path = pg_catalog, public, app_private, pg_temp
  AS $$
declare
  event_ids bigint[];
  person_ids bigint[];
begin
  select array_agg(distinct target.event_id)
  into event_ids
  from event_target_cohort target
  join event instance on instance.id = target.event_id
  where target.cohort_id = any (
    case when tg_op = 'INSERT' then array[new.cohort_id]
      when tg_op = 'DELETE' then array[old.cohort_id]
      else array[old.cohort_id, new.cohort_id]
    end
  ) and instance.since >= now();

  if event_ids is not null then
    person_ids := case when tg_op = 'INSERT' then array[new.person_id]
      when tg_op = 'DELETE' then array[old.person_id]
      else array[old.person_id, new.person_id]
    end;
    perform app_private.reconcile_event_cohort_registrations(event_ids, person_ids);
  end if;

  return case when tg_op = 'DELETE' then old else new end;
end;
$$;

grant all on function app_private.tg_cohort_membership__on_status to trainer;

DROP TRIGGER IF EXISTS _500_on_status ON cohort_membership;
CREATE TRIGGER _500_on_status
  AFTER INSERT OR DELETE OR UPDATE OF status, cohort_id, person_id, since, until
  ON cohort_membership
  FOR EACH ROW EXECUTE FUNCTION app_private.tg_cohort_membership__on_status();
--! EndIncluded functions/tg_cohort_membership__on_status.sql
--! Included functions/event_registration_info.sql
drop function if exists event_instance_registration_info;
drop function if exists event_registration_info;
drop type if exists event_instance_registration_info;
drop type if exists event_registration_info;

create type event_registration_info as (
  registrations integer,
  people integer,
  remaining_capacity integer,
  my boolean
);

create or replace function event_registration_info(inst event)
  returns event_registration_info
  language sql stable security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select row(
    count(*) filter (where parent_registration_id is null)::integer,
    count(person_id)::integer,
    case
      when inst.capacity is null or inst.capacity <= 0 then null
      else inst.capacity
        - case inst.capacity_unit
            when 'people' then count(person_id)::integer
            when 'registrations' then count(*) filter (where parent_registration_id is null)::integer
          end
    end,
    count(*) filter (where person_id = any(current_person_ids())) > 0
  )::event_registration_info
  from event_registration
  where event_id = inst.id and registration_status = 'active';
$$;

grant all on function event_registration_info to anonymous;
--! EndIncluded functions/event_registration_info.sql
--! Included functions/event_remaining_x.sql
drop function if exists event_instance_remaining_person_spots;

drop function if exists event_instance_trainer_lessons_remaining;

CREATE or replace FUNCTION event_trainer_lessons_remaining(e event_trainer) RETURNS integer AS $$
  select case
    when e.lessons_offered is null then null
    else e.lessons_offered - (
      select coalesce(sum(demand.lesson_count), 0)
      from event_lesson_request demand
      join event_registration registration
        on registration.id = demand.registration_id
       and registration.registration_status = 'active'
      where demand.trainer_id = e.id
    )
  end;
$$ LANGUAGE sql STABLE security definer set search_path = pg_catalog, public, pg_temp;

CREATE or replace FUNCTION event_remaining_person_spots(inst event) RETURNS integer AS $$
  select case
    when inst.capacity is null or inst.capacity <= 0 then null
    else inst.capacity - case inst.capacity_unit
        when 'people' then (
          select count(*)::integer
          from event_registration registration
          where registration.event_id = inst.id
            and registration.person_id is not null
            and registration.registration_status = 'active'
        )
        when 'registrations' then (
          select count(*)::integer
          from event_registration registration
          where registration.event_id = inst.id
            and registration.parent_registration_id is null
            and registration.registration_status = 'active'
        )
      end
  end;
$$ LANGUAGE sql STABLE security definer set search_path = pg_catalog, public, pg_temp;

GRANT ALL ON FUNCTION event_trainer_lessons_remaining(e event_trainer) TO anonymous;
GRANT ALL ON FUNCTION event_remaining_person_spots(inst event) TO anonymous;
--! EndIncluded functions/event_remaining_x.sql

create index if not exists event_registration_event_id_id_idx on event_registration (event_id, id);
drop index if exists event_registration_event_id_idx;
