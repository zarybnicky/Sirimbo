--! Previous: sha1:5281d8e058b51919d30bc69910213a58cc22cbdb
--! Hash: sha1:cf6a239e9abe6635faf1d2ef239507a520d03d72

--! split: 1-current.sql
CREATE or replace FUNCTION register_to_event_many(registrations register_to_event_type[]) RETURNS SETOF event_registration
    LANGUAGE plpgsql
    AS $$
declare
  event event;
  created_ids bigint[] := array[]::bigint[];
  registration register_to_event_type;
  created event_registration;
  demand event_lesson_demand;
begin
  foreach registration in array registrations loop
    select * into event from event where id = registration.event_id;

    if event is null then
      raise exception 'EVENT_NOT_FOUND' using errcode = '28000';
    end if;
    if event.is_locked = true then
      raise exception 'NOT_ALLOWED' using errcode = '28000';
    end if;
    if registration.person_id <> all (current_person_ids()) and registration.couple_id <> all (current_couple_ids()) then
      raise exception 'ACCESS_DENIED' using errcode = '42501';
    end if;

    insert into event_registration (event_id, person_id, couple_id, note)
    values (registration.event_id, registration.person_id, registration.couple_id, registration.note)
    returning * into created;
    created_ids := created_ids || created.id;
    if registration.lessons is not null then
      foreach demand in array registration.lessons loop
        perform set_lesson_demand(created.id, demand.trainer_id, demand.lesson_count);
      end loop;
    end if;
  end loop;
  return query select * from event_registration where id = any (created_ids);
end;
$$ security definer;

select verify_function('public.register_to_event_many');
GRANT ALL ON FUNCTION public.register_to_event_many TO anonymous;

create or replace function public.event_instance_approx_price(v_instance event_instance) returns table (amount numeric(19, 4), currency text) language plpgsql stable as $$
declare
  num_participants bigint;
  duration numeric;
begin
  num_participants := (select count(*) from event join lateral event_registrants(event.*) on true where event.id=v_instance.event_id);
  duration = extract(epoch from (v_instance.until - v_instance.since)) / 60;

  if exists (select 1 from event_instance_trainer where instance_id = v_instance.id) then
    return query
    select
      coalesce(sum((tenant_trainer.member_price_45min).amount * duration / 45 / nullif(num_participants, 0))::numeric(19,4), 'NaN') as amount,
      coalesce((tenant_trainer.member_price_45min).currency, 'CZK') as currency
    from event_instance_trainer join tenant_trainer on event_instance_trainer.person_id=tenant_trainer.person_id and status='active'
    where event_instance_trainer.instance_id=v_instance.id and tenant_trainer.tenant_id = event_instance_trainer.tenant_id
    group by (tenant_trainer.member_price_45min).currency;
  else
    return query
    select
      coalesce(sum((tenant_trainer.member_price_45min).amount * duration / 45 / nullif(num_participants, 0))::numeric(19,4), 'NaN') as amount,
      coalesce((tenant_trainer.member_price_45min).currency, 'CZK') as currency
    from event_trainer join tenant_trainer on event_trainer.person_id=tenant_trainer.person_id and status='active'
    where event_trainer.event_id=v_instance.event_id and tenant_trainer.tenant_id = event_trainer.tenant_id
    group by (tenant_trainer.member_price_45min).currency;
  end if;
end;
$$;

select verify_function('event_instance_approx_price');

grant all on function event_instance_approx_price to anonymous;

COMMENT ON FUNCTION event_instance_approx_price IS '@simpleCollections only';

CREATE or replace FUNCTION public.reset_password(email character varying) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  v_tenant tenant;
  v_user users;
  v_token otp_token;
  v_payload jsonb := null;
begin
  for v_user in (select * from users where u_email = email) loop
    insert into otp_token (user_id)
    values (v_user.id) returning * into v_token;

    v_payload := coalesce(v_payload, jsonb_build_array()) || jsonb_build_object(
      'login', v_user.u_login,
      'email', v_user.u_email,
      'token', v_token.access_token,
      'people', (
        select jsonb_agg(person_name(person.*))
        from user_proxy join person on person_id=person.id
        where status = 'active' and user_id = v_user.id
      )
    );
  end loop;

  select * into v_tenant from tenant where id = current_tenant_id();

  if v_payload is not null then
    perform graphile_worker.add_job('forgotten_password_generate', json_build_object(
      'origin', v_tenant.origins[1],
      'intent', '/zapomenute-heslo',
      'users', v_payload
    ));
  end if;
end;
$$;

GRANT ALL ON FUNCTION public.reset_password(email character varying) TO anonymous;
select verify_function('reset_password');


drop function if exists register_to_event;
drop function if exists my_event_instance_for_range;

alter table aktuality drop column if exists at_foto_main;
drop table if exists app_private.galerie_foto;
drop table if exists app_private.galerie_dir;

alter table event alter column capacity SET default 0;
alter table crawler.json_response_cache alter content set not null;
alter table crawler.html_response_cache alter content set not null;

alter table tenant_administrator alter until set default 'infinity';
update tenant_administrator set until = 'infinity' where until is null;
alter table tenant_administrator
  alter until set not null,
  drop constraint if exists tenant_administrator_until_gt_since,
  add constraint tenant_administrator_until_gt_since check (until > since);

alter table tenant_trainer alter until set default 'infinity';
update tenant_trainer set until = 'infinity' where until is null;
alter table tenant_trainer
  alter until set not null,
  drop constraint if exists tenant_trainer_until_gt_since,
  add constraint tenant_trainer_until_gt_since check (until > since);

alter table tenant_membership alter until set default 'infinity';
update tenant_membership set until = 'infinity' where until is null;
alter table tenant_membership
  alter until set not null,
  drop constraint if exists tenant_membership_until_gt_since,
  add constraint tenant_membership_until_gt_since check (until > since);

alter table cohort_membership alter until set default 'infinity';
update cohort_membership set until = 'infinity' where until is null;
alter table cohort_membership
  alter until set not null,
  drop constraint if exists cohort_membership_until_gt_since,
  add constraint cohort_membership_until_gt_since check (until > since);

alter table couple alter until set default 'infinity';
update couple set until = 'infinity' where until is null;
alter table couple
  alter until set not null,
  drop constraint if exists couple_until_gt_since,
  add constraint couple_until_gt_since check (until > since);

alter table user_proxy
  alter until set default 'infinity',
  alter since set default now();
update user_proxy set since = '-infinity' where since is null;
update user_proxy set until = 'infinity' where until is null;
alter table user_proxy
  alter since set not null,
  alter until set not null,
  drop constraint if exists user_proxy_until_gt_since,
  add constraint user_proxy_until_gt_since check (until > since);

alter table posting alter amount set not null;

alter table users drop if exists u_ban;
alter table tenant_location alter is_public set not null;
