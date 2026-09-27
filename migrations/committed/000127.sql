--! Previous: sha1:790e4174be3c995f46e169e95568b646c8d108e3
--! Hash: sha1:dff0fe4ee3a3d0e0374530cb131e6775ad6009f7

--! split: 1-current.sql
grant all on membership_application to anonymous;
grant execute on function app_private.normalize_name to anonymous;
grant execute on function app_private.relationship_status_next to administrator;

--! Included functions/current_account_ids.sql
create or replace function current_account_ids() returns setof bigint
  language sql stable
as $$
  select id from account
  where person_id = any ((select current_person_ids())::bigint[]);
$$;

comment on function current_account_ids() is '@omit';
grant execute on function current_account_ids() to anonymous;
--! EndIncluded functions/current_account_ids.sql
--! Included functions/visible_payment_ids.sql
create or replace function app_private.visible_payment_ids() returns setof bigint
  language sql stable security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select payment_id from public.payment_debtor
  where tenant_id = (select current_tenant_id())
    and person_id = any ((select current_person_ids())::bigint[])
  union
  select payment_id from public.payment_recipient
  where tenant_id = (select current_tenant_id())
    and account_id = any (array(select current_account_ids()));
$$;

grant execute on function app_private.visible_payment_ids() to anonymous;
--! EndIncluded functions/visible_payment_ids.sql
--! Included policies/accounting.sql
select app_private.drop_policies('public.account');
create policy current_tenant on account as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_manage on account to administrator using (true);
create policy member_view on account for select to member
  using (person_id = any ((select current_person_ids())::bigint[]));

select app_private.drop_policies('public.payment');
create policy current_tenant on payment as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_manage on payment to administrator using (true);
create policy member_view on payment for select to member
  using (id = any (array(select app_private.visible_payment_ids())));

select app_private.drop_policies('public.payment_debtor');
create policy current_tenant on payment_debtor as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_manage on payment_debtor to administrator using (true);
create policy member_view on payment_debtor for select to member
  using (person_id = any ((select public.current_person_ids())::bigint[]));

select app_private.drop_policies('payment_recipient');
create policy current_tenant on payment_recipient as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_manage on payment_recipient to administrator using (true);
create policy member_view on payment_recipient for select to member
  using (account_id = any (array(select public.current_account_ids())));

select app_private.drop_policies('posting');
create policy current_tenant on posting as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_manage on posting to administrator using (true);
create policy member_view on posting for select to member
  using (account_id = any (array(select current_account_ids())));

select app_private.drop_policies('public.transaction');
create policy current_tenant on transaction as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_manage on transaction to administrator using (true);
create policy member_view on transaction for select to member
  using (id = any (array(select transaction_id from posting))
    or payment_id = any (array(select app_private.visible_payment_ids())));

grant all on account, payment, payment_debtor, payment_recipient, posting, transaction to anonymous;
--! EndIncluded policies/accounting.sql
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

comment on function payment_debtor_price is '@simpleCollections only';
grant all on function payment_debtor_price to anonymous;
--! EndIncluded functions/payment_debtor_price.sql
--! Included policies/event_instance.sql
select app_private.drop_policies('public.event_instance');

create policy current_tenant on event_instance as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_same_tenant on event_instance to administrator using (true);
create policy trainer_select on event_instance for select to trainer
  using (cardinality(manager_person_ids) = 0 or manager_person_ids && (select current_person_ids()));
create policy trainer_insert on event_instance for insert to trainer
  with check (parent_id is null or app_private.can_trainer_edit_instance(parent_id));
create policy trainer_update on event_instance for update to trainer
  using (app_private.can_trainer_edit_instance(id));
create policy trainer_delete on event_instance for delete to trainer
  using (app_private.can_trainer_edit_instance(id));
create policy member_view on event_instance for select to member
  using (is_visible);
create policy public_view on event_instance for select to anonymous
  using (is_public);
create policy event_share_view on event_instance for select to anonymous
  using (id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[]));
--! EndIncluded policies/event_instance.sql
--! Included policies/event_instance_trainer.sql
select app_private.drop_policies('public.event_instance_trainer');

create policy current_tenant on event_instance_trainer as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on event_instance_trainer to administrator using (true);
create policy trainer_same_tenant on event_instance_trainer to trainer
  using (app_private.can_trainer_edit_instance(instance_id));
create policy member_view on event_instance_trainer for select to member using (true);
create policy event_share_view on event_instance_trainer for select to anonymous
  using (instance_id = any ((select current_setting('jwt.claims.shared.event_ids', true))::bigint[]));
--! EndIncluded policies/event_instance_trainer.sql
--! Included policies/event_external_registration.sql
select app_private.drop_policies('public.event_external_registration');

CREATE POLICY current_tenant ON event_external_registration AS RESTRICTIVE
  USING (tenant_id = (SELECT current_tenant_id()));
CREATE POLICY admin_all ON event_external_registration TO administrator USING (true);
CREATE POLICY register_public ON event_external_registration FOR INSERT TO anonymous
  WITH CHECK ((SELECT instance.is_public FROM event_instance instance WHERE instance_id = instance.id));
CREATE POLICY trainer_same_tenant ON event_external_registration TO trainer
  USING (app_private.can_trainer_edit_instance(instance_id));
CREATE POLICY view_visible_instance ON event_external_registration FOR SELECT TO member
  USING (instance_id = any (SELECT id from event_instance));
CREATE POLICY admin_my ON event_external_registration TO member
  USING ((SELECT not instance.is_locked FROM event_instance instance WHERE instance_id = instance.id)
     AND (created_by = current_user_id()));
--! EndIncluded policies/event_external_registration.sql
--! Included policies/event_lesson_demand.sql
select app_private.drop_policies('public.event_lesson_demand');

CREATE POLICY current_tenant ON event_lesson_demand AS RESTRICTIVE
  USING (tenant_id = (SELECT current_tenant_id()));
CREATE POLICY admin_all ON event_lesson_demand TO administrator USING (true);
CREATE POLICY view_visible_instance ON event_lesson_demand FOR SELECT
  USING (registration_id IN (SELECT id FROM event_instance_registration));

GRANT ALL ON event_lesson_demand TO anonymous;
--! EndIncluded policies/event_lesson_demand.sql
--! Included policies/announcement.sql
select app_private.drop_policies('public.announcement');

create policy current_tenant on announcement as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on announcement to administrator using (true);
create policy trainer_manage_own on announcement to trainer
  using (author_id = (select current_user_id()));
create policy member_view on announcement for select to member
  using (id in (select app_private.visible_announcement_ids()));

select app_private.drop_policies('public.announcement_audience');

create policy current_tenant on announcement_audience as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on announcement_audience to administrator using (true);
create policy trainer_manage on announcement_audience to trainer
  using (announcement_id in (select id from announcement where author_id = (select current_user_id())));
create policy member_view on announcement_audience for select to member using (true);

grant all on table announcement, announcement_audience to anonymous;
--! EndIncluded policies/announcement.sql
--! Included functions/save_events.sql
create or replace function save_events(
  details event_details_input,
  events event_input[],
  trainers event_trainer_input[] default '{}'::event_trainer_input[],
  cohort_ids bigint[] default '{}'::bigint[],
  series event_series_input default null
) returns setof event_instance
  language plpgsql
as $$
declare
  event_to_save event_input;
  v_saved_event event_instance;
  v_saved_event_ids bigint[] := '{}'::bigint[];
  v_tenant_id bigint := current_tenant_id();
  v_series_id bigint;
  v_assign_series boolean := (series).id is not null or (series).name is not null;
  v_is_visible boolean := coalesce((details).is_visible, true);
  v_is_public boolean := coalesce((details).is_public, false);
  v_has_public_details boolean := v_is_public and coalesce((details).has_public_details, false);
  v_is_locked boolean := coalesce((details).is_locked, false);
  v_enable_notes boolean := coalesce((details).enable_notes, false);
  v_expected_existing_count bigint;
  v_locked_existing_count bigint;
begin
  if details is null then
    raise exception 'event details are required';
  end if;

  if (details).type is null
    or (details).capacity is null
    or (details).capacity < 0
    or (details).capacity_unit is null then
    raise exception 'event details are incomplete';
  end if;

  if cardinality(coalesce(events, '{}'::event_input[])) = 0 then
    raise exception 'at least one event is required';
  end if;

  if exists (
    select 1 from unnest(events) i
    where i is null or i.since is null or i.until is null or i.until <= i.since
  ) then
    raise exception 'every event requires a valid time range';
  end if;

  if exists (select i.id from unnest(events) i where i.id is not null group by i.id having count(*) > 1) then
    raise exception 'an event may only be submitted once';
  end if;

  if exists (
    select 1
    from unnest(events) i
    cross join lateral unnest(coalesce(i.registrations, '{}'::event_registration_input[])) registration
    where (registration.person_id is null) = (registration.couple_id is null)
  ) then
    raise exception 'an event registration requires exactly one person or couple';
  end if;

  if exists (
    select 1
    from unnest(coalesce(trainers, '{}'::event_trainer_input[])) trainer
    where trainer.person_id is null or trainer.lessons_offered < 0
  ) then
    raise exception 'an event trainer requires a person and a non-negative lesson limit';
  end if;

  if exists (
    select 1
    from unnest(coalesce(cohort_ids, '{}'::bigint[])) i(cohort_id)
    left join cohort cohort on cohort.id = i.cohort_id and cohort.tenant_id = v_tenant_id
    where i.cohort_id is not null and cohort.id is null
  ) then
    raise exception 'event cohort not found';
  end if;

  if exists (
    select 1 from unnest(events) i where i.id is null
  ) and (details).parent_id is not null and not exists (
    select 1
    from event_instance parent
    where parent.id = (details).parent_id and parent.tenant_id = v_tenant_id
  ) then
    raise exception 'event parent % not found or not editable', (details).parent_id;
  end if;

  if v_assign_series then
    if (series).id is null then
      insert into event_series (name)
      values (coalesce((series).name, (details).name))
      returning id into v_series_id;
    else
      select e.id into v_series_id
      from event_series e where e.id = (series).id and e.tenant_id = v_tenant_id
      for update;

      if not found then
        raise exception 'event series % not found or not editable', (series).id;
      end if;
    end if;
  end if;

  select count(*) into v_expected_existing_count from unnest(events) i where i.id is not null;

  perform e.id
  from event_instance e
  join unnest(events) i on i.id = e.id
  where i.id is not null and e.tenant_id = v_tenant_id
  order by e.id
  for update of e;

  get diagnostics v_locked_existing_count = row_count;
  if v_locked_existing_count <> v_expected_existing_count then
    raise exception 'one or more events were not found or are not editable';
  end if;

  foreach event_to_save in array events loop
    if event_to_save.id is null then
      insert into event_instance (
        parent_id,
        series_id,
        since,
        until,
        is_cancelled,
        name,
        type,
        location_id,
        location_text,
        capacity,
        capacity_unit,
        is_visible,
        is_public,
        has_public_details,
        is_locked,
        enable_notes,
        description,
        summary,
        files_legacy
      ) values (
        (details).parent_id,
        v_series_id,
        event_to_save.since,
        event_to_save.until,
        coalesce(event_to_save.is_cancelled, false),
        (details).name,
        (details).type,
        (details).location_id,
        coalesce((details).location_text, ''),
        (details).capacity,
        (details).capacity_unit,
        v_is_visible,
        v_is_public,
        v_has_public_details,
        v_is_locked,
        v_enable_notes,
        '',
        '',
        ''
      )
      returning * into v_saved_event;
    else
      update event_instance e
      set since = event_to_save.since,
          until = event_to_save.until,
          is_cancelled = coalesce(event_to_save.is_cancelled, false),
          name = (details).name,
          type = (details).type,
          location_id = (details).location_id,
          location_text = coalesce((details).location_text, ''),
          capacity = (details).capacity,
          capacity_unit = (details).capacity_unit,
          is_visible = v_is_visible,
          is_public = v_is_public,
          has_public_details = v_has_public_details,
          is_locked = v_is_locked,
          enable_notes = v_enable_notes,
          series_id = case
            when v_assign_series then v_series_id
            else e.series_id
          end
      where e.id = event_to_save.id and e.tenant_id = v_tenant_id
      returning * into v_saved_event;

      if not found then
        raise exception 'event % not found or not editable', event_to_save.id;
      end if;
    end if;

    v_saved_event_ids := array_append(v_saved_event_ids, v_saved_event.id);

    perform registration.id
    from event_instance_registration registration
    where registration.instance_id = v_saved_event.id
    order by registration.id
    for update;

    with desired as (
      select distinct registration.person_id, registration.couple_id
      from unnest(coalesce(event_to_save.registrations, '{}'::event_registration_input[])) registration
    ), roots as (
      select e.id
      from event_instance_registration e
      where e.instance_id = v_saved_event.id
        and e.parent_registration_id is null
        and not exists (
          select 1 from desired
          where desired.person_id is not distinct from e.person_id
            and desired.couple_id is not distinct from e.couple_id
        )
    )
    update event_instance_registration registration
    set registration_status = 'cancelled',
        target_cohort_id = null,
        source = case when registration.id = roots.id
          then 'manager'::event_registration_source end
    from roots
    where registration.registration_status <> 'cancelled'
      and (registration.id = roots.id or registration.parent_registration_id = roots.id);

    with desired as (
      select distinct registration.person_id, registration.couple_id
      from unnest(
        coalesce(event_to_save.registrations, '{}'::event_registration_input[])
      ) registration
    ), roots as (
      select e.id
      from event_instance_registration e
      join desired
        on desired.person_id is not distinct from e.person_id
        and desired.couple_id is not distinct from e.couple_id
      where e.instance_id = v_saved_event.id
        and e.parent_registration_id is null
    )
    update event_instance_registration registration
    set registration_status = 'active',
        target_cohort_id = null,
        source = case when registration.id = roots.id
          then 'manager'::event_registration_source end
    from roots
    where registration.registration_status <> 'active'
      and (registration.id = roots.id or registration.parent_registration_id = roots.id);

    with desired as (
      select distinct registration.person_id, registration.couple_id
      from unnest(
        coalesce(event_to_save.registrations, '{}'::event_registration_input[])
      ) registration
    ), roots as (
      insert into event_instance_registration (
        instance_id, person_id, couple_id, source, status
      )
      select v_saved_event.id,
        desired.person_id,
        desired.couple_id,
        'manager',
        case when desired.person_id is not null
          then 'unknown'::attendance_type end
      from desired
      where not exists (
        select 1
        from event_instance_registration e
        where e.instance_id = v_saved_event.id
          and e.parent_registration_id is null
          and e.person_id is not distinct from desired.person_id
          and e.couple_id is not distinct from desired.couple_id
      )
      returning id, couple_id
    )
    insert into event_instance_registration (instance_id, parent_registration_id, person_id, status)
    select v_saved_event.id, roots.id, person.person_id, 'unknown'
    from roots
    join couple couple on couple.id = roots.couple_id
    cross join lateral unnest(array[couple.man_id, couple.woman_id]) person(person_id);
  end loop;

  with desired as (
    select distinct i.cohort_id
    from unnest(coalesce(cohort_ids, '{}'::bigint[])) i(cohort_id)
    where i.cohort_id is not null
  )
  insert into event_instance_target_cohort (tenant_id, instance_id, cohort_id)
  select stored_event.tenant_id, stored_event.id, desired.cohort_id
  from event_instance stored_event
  join unnest(v_saved_event_ids) saved(id) on saved.id = stored_event.id
  cross join desired
  on conflict (instance_id, cohort_id) do nothing;

  delete from event_instance_target_cohort e
  where e.instance_id = any(v_saved_event_ids)
    and not exists (
      select 1
      from unnest(coalesce(cohort_ids, '{}'::bigint[])) i(cohort_id)
      where i.cohort_id = e.cohort_id
    );

  -- Keep the caller's trainer assignment until all edits and replacements are saved.
  with desired as (
    select distinct on (trainer.person_id) trainer.person_id, trainer.lessons_offered
    from unnest(coalesce(trainers, '{}'::event_trainer_input[]))
      with ordinality trainer(person_id, lessons_offered, position)
    order by trainer.person_id, trainer.position
  )
  insert into event_instance_trainer (tenant_id, instance_id, person_id, lessons_offered)
  select stored_event.tenant_id, stored_event.id, desired.person_id, desired.lessons_offered
  from event_instance stored_event
  join unnest(v_saved_event_ids) saved(id) on saved.id = stored_event.id
  cross join desired
  on conflict (instance_id, person_id) do update
  set lessons_offered = excluded.lessons_offered;

  delete from event_instance_trainer e
  where e.instance_id = any(v_saved_event_ids)
    and not exists (
      select 1
      from unnest(coalesce(trainers, '{}'::event_trainer_input[])) trainer
      where trainer.person_id = e.person_id
    );

  return query
  select stored_event.*
  from unnest(v_saved_event_ids) with ordinality saved(event_id, position)
  join event_instance stored_event on stored_event.id = saved.event_id
  order by saved.position;
end;
$$;

comment on function save_events is '@simpleCollections only';
grant execute on function save_events to anonymous;
--! EndIncluded functions/save_events.sql
--! Included functions/visible_user_proxy_ids.sql
create or replace function app_private.visible_user_proxy_ids() returns setof bigint
  language sql stable security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select id from user_proxy
  where status = 'active'
    and person_id in (
      select person_id from user_proxy
      where user_id = (select current_user_id()) and status = 'active'
    );
$$;

grant execute on function app_private.visible_user_proxy_ids() to anonymous;
--! EndIncluded functions/visible_user_proxy_ids.sql
--! Included policies/users.sql
select app_private.drop_policies('public.user_proxy');
create policy admin_all on user_proxy to administrator using (true);
create policy view_personal on user_proxy for select
  using (user_id = (select current_user_id())
    or id = any (array(select app_private.visible_user_proxy_ids())));

select app_private.drop_policies('public.users');
create policy admin_all on users to administrator using (true);
create policy manage_own on users using (id = (select current_user_id()));
create policy view_shared_person on users for select
  using (id = any (array(select user_id from user_proxy where status = 'active')));

grant all on users, user_proxy to anonymous;
--! EndIncluded policies/users.sql
--! Included functions/announcement_author_name.sql
create or replace function announcement_author_name(a announcement) returns text
  language sql stable security definer
  set search_path = pg_catalog, public, pg_temp
as $$
  select concat_ws(' ', nullif(u.u_jmeno, ''), nullif(u.u_prijmeni, ''))
  from announcement stored
  join users u on u.id = stored.author_id
  where stored.id = a.id;
$$;

grant execute on function announcement_author_name(announcement) to anonymous;
--! EndIncluded functions/announcement_author_name.sql
--! Included policies/membership_application.sql
select app_private.drop_policies('public.membership_application');
create policy current_tenant on membership_application as restrictive using (tenant_id = (select current_tenant_id()));
create policy manage_admin on membership_application to administrator using (true);
create policy view_my on membership_application for select using (created_by = (select current_user_id()));
create policy insert_my on membership_application for insert
  with check (created_by = (select current_user_id()) and status in ('new', 'sent'));
create policy update_my on membership_application for update
  using (created_by = (select current_user_id()) and status in ('new', 'sent'))
  with check (created_by = (select current_user_id()) and status in ('new', 'sent'));
create policy delete_my on membership_application for delete
  using (created_by = (select current_user_id()) and status in ('new', 'sent'));
--! EndIncluded policies/membership_application.sql
