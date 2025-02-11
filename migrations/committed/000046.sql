--! Previous: sha1:6d70a5dbb92e5acc2a9d1fa07b49194ea811e0b9
--! Hash: sha1:5746c7232755638f52c833a6008964ed30fe2f5b

--! split: 1-current.sql
grant all on function immutable_concat_ws to anonymous;
grant all on function http_header to anonymous;
grant all on function http to trainer;
grant all on function fetch_with_cache to trainer;

CREATE or replace FUNCTION app_private.tg_cohort_membership__on_status() RETURNS trigger LANGUAGE plpgsql AS $$
begin
  if NEW.status = 'expired' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then
    -- with affected as (
    --   select cohort_membership.until, event_registration.person_id, event_registration.id as registration_id
    --   from event_target_cohort
    --   join event_registration on event_target_cohort.event_id=event_registration.event_id
    --   join cohort_membership on cohort_membership.person_id=event_registration.person_id and cohort_membership.cohort_id=event_target_cohort.cohort_id
    --   where cohort_membership.until is not null
    --     and cohort_membership.id = OLD.id
    -- )
    -- update event_attendance set status = 'cancelled'
    -- where id in (
    --   select event_attendance.id
    --   from event_attendance
    --   join event_instance on event_instance.id = event_attendance.instance_id
    --   join affected on event_attendance.registration_id = affected.registration_id
    --    and affected.until < event_instance.since
    --    and status in ('unknown', 'not-excused', 'excused')
    -- );

    -- with affected as (
    --   select cohort_membership.until, event_registration.person_id, event_registration.id as registration_id
    --   from event_target_cohort
    --   join event_registration on event_target_cohort.event_id=event_registration.event_id
    --   join cohort_membership on cohort_membership.person_id=event_registration.person_id and cohort_membership.cohort_id=event_target_cohort.cohort_id
    --   where cohort_membership.until is not null
    --   and cohort_membership.id = OLD.id
    -- )
    -- delete from event_registration
    -- where exists (
    --   select event_attendance.id
    --   from event_attendance
    --   join event_instance on event_instance.id = event_attendance.instance_id
    --   join affected on event_attendance.registration_id = affected.registration_id
    --    and affected.until < event_instance.since
    --    and status = 'cancelled'
    -- ) and not exists (
    --   select event_attendance.id
    --   from event_attendance
    --   join event_instance on event_instance.id = event_attendance.instance_id
    --   join affected on event_attendance.registration_id = affected.registration_id
    --    and affected.until < event_instance.since
    --    and status <> 'cancelled'
    -- );

  elsif NEW.status = 'active' and (TG_OP = 'INSERT' or OLD.status <> NEW.status) then
    -- add payments
    perform app_private.register_new_cohort_member_to_events(NEW);
  end if;
  return NEW;
end;
$$ security definer;

grant all on function app_private.tg_cohort_membership__on_status to trainer;
select verify_function('app_private.tg_cohort_membership__on_status', 'cohort_membership');

CREATE or replace FUNCTION public.submit_form(type text, data jsonb, url text) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
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

GRANT ALL ON FUNCTION public.submit_form(type text, data jsonb, url text) TO anonymous;
select verify_function('submit_form');



create or replace function app_private.merge_couples(one bigint, two bigint) returns void language plpgsql as $$
declare
  registrations_before bigint;
  registrations_after bigint;
begin
  -- duplicate detection: select array_agg(couple.id), man_id, woman_id, array_agg(array[since, until] order by since) from couple group by man_id, woman_id having count(*) > 1;

  select count(*) into registrations_before from event_registration;

  assert (select min(man_id) is not distinct from max(man_id) from couple where id in (one, two));
  assert (select min(woman_id) is not distinct from max(woman_id) from couple where id in (one, two));
  assert (select extract(epoch from (select since from couple where id = two) - (select until from couple where id = one)) < 3600);

  update event_registration set couple_id = one where couple_id = two;
  update couple set until = (select until from couple where id = two) where id = one;
  delete from couple where id = two;

  select count(*) into registrations_after from event_registration;
  assert registrations_before = registrations_after;
end
$$;

do $$ begin
  if not exists (select 1 from pg_type where typname = 'address_type' and typcategory = 'C') then
    create type address_type as (
      street text,
      conscription_number text,
      orientation_number text,
      district text,
      city text,
      region text,
      postal_code text
    );
    create domain address_domain as address_type check (
      value is null or (
        (value).street is not null and
        (value).conscription_number is not null and
        (value).orientation_number is not null and
        (value).city is not null and
        (value).region is not null and
        (value).postal_code is not null
      )
    );
  end if;
end $$;

alter table person add column if not exists address address_domain;

do $$ begin
  if exists (select 1 from pg_tables where tablename = 'legacy_users' and true) then
    update person set address = (u_street, u_conscription_number, u_orientation_number, u_district, u_city, '', u_postal_code)
      from legacy_users where legacy_user_id = u_id and address is null;
    drop table legacy_users;
  end if;
end $$;

comment on table accounting_period is '@omit';
comment on type jwt_token is '@jwt';

comment on function register_to_event is E'@arg0variant input
@arg1variant patch';

COMMENT ON FUNCTION create_event(INOUT info event, instances event_instance[], trainers event_trainer[], cohorts event_target_cohort[], registrations event_registration[]) IS '@arg0variant input
@arg1variant patch
@arg2variant patch
@arg3variant patch
@arg4variant patch
';

do $$
begin
  if exists (select 1 from information_schema.columns where table_schema = 'public' and table_name = 'location' and column_name = 'location') then
    alter table room rename location to location_id;
  end if;
end;
$$;

drop function if exists payment_debtor_price_to_pay;
drop function if exists payment_debtor_price;
CREATE or replace FUNCTION public.payment_debtor_price(p public.payment_debtor) RETURNS price_type LANGUAGE sql STABLE as $$
  SELECT (
    sum(payment_recipient.amount) / (
      SELECT count(*) AS count
      FROM public.payment_debtor
      WHERE p.payment_id = payment_debtor.payment_id
    )::numeric(19,4),
    min(account.currency)::text
  )::price
  FROM payment_recipient
  JOIN account ON payment_recipient.account_id = account.id
  WHERE payment_recipient.payment_id = p.payment_id;
$$;

comment on function payment_debtor_price is '@simpleCollections only';
GRANT ALL ON FUNCTION public.payment_debtor_price(p public.payment_debtor) TO anonymous;

DO $$ BEGIN
  IF EXISTS (SELECT FROM pg_catalog.pg_aggregate join pg_proc on aggfnoid=pg_proc.oid WHERE proname='array_accum' and pronamespace::regnamespace::text = 'public') THEN
    drop AGGREGATE array_accum (anycompatiblearray);
  END IF;

  IF NOT EXISTS (SELECT FROM pg_catalog.pg_aggregate join pg_proc on aggfnoid=pg_proc.oid WHERE proname='array_accum' and pronamespace::regnamespace::text = 'app_private') THEN
    CREATE AGGREGATE app_private.array_accum (anycompatiblearray) (
      sfunc = array_cat,
      stype = anycompatiblearray,
      initcond = '{}'
    );
  END IF;
END $$;

CREATE or replace FUNCTION app_private.create_jwt_token(u public.users) RETURNS public.jwt_token
    LANGUAGE sql STABLE
    AS $$
  with details as (
    SELECT
      u_id,
      user_proxy.person_id as person_id,
      tenant_memberships || tenant_trainers || tenant_administrators as my_tenant_ids,
      cohort_memberships as my_cohort_ids,
      couple_ids as my_couple_ids,
      current_tenant_id() = ANY (tenant_memberships || tenant_trainers || tenant_administrators) as is_member,
      current_tenant_id() = ANY (tenant_trainers) as is_trainer,
      current_tenant_id() = ANY (tenant_administrators) as is_admin
    from users
    left join user_proxy on user_id=users.u_id
    left join auth_details on user_proxy.person_id=auth_details.person_id
    where users.u_id=u.u_id
  ) select
    extract(epoch from now() + interval '7 days')::integer,
    u.u_id,
    current_tenant_id(),
    u.u_login,
    u.u_email,
    array_to_json(array_agg(person_id)) as my_person_ids,
    array_to_json(app_private.array_accum(my_tenant_ids)) as my_tenant_ids,
    array_to_json(app_private.array_accum(my_cohort_ids)) as my_cohort_ids,
    array_to_json(app_private.array_accum(my_couple_ids)) as my_couple_ids,
    bool_or(is_member) as is_member,
    bool_or(is_trainer) as is_trainer,
    bool_or(is_admin) as is_admin
  from details group by u_id;
$$;

--
comment on constraint upozorneni_up_kdo_fkey on upozorneni is
  E'@fieldName userByUpKdo';
comment on constraint dokumenty_d_kdo_fkey on dokumenty is
  E'@fieldName userByDKdo';
comment on constraint aktuality_at_foto_main_fkey on aktuality is
  E'@fieldName galerieFotoByAtFotoMain';
comment on constraint upozorneni_skupiny_ups_id_skupina_fkey on upozorneni_skupiny is
  E'@fieldName cohortByUpsIdSkupina';
