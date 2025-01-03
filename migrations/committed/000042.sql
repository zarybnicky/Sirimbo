--! Previous: sha1:77545a9aba9a9b99fcfea9eb6234b6ab0b765683
--! Hash: sha1:838b327aa46c6c4716b50750b40519024850ecfb

--! split: 1-current.sql
CREATE or replace FUNCTION public.filtered_people(is_trainer boolean, is_admin boolean, in_cohorts bigint[] default null) RETURNS SETOF person LANGUAGE sql STABLE AS $$
  select person.* from person
  join auth_details on person_id=person.id
  where
    current_tenant_id() = any (auth_details.allowed_tenants)
    and case when in_cohorts is null then true else in_cohorts = auth_details.cohort_memberships OR in_cohorts && auth_details.cohort_memberships end
    and case when is_trainer is null then true else is_trainer = (current_tenant_id() = any (auth_details.tenant_trainers)) end
    and case when is_admin is null then true else is_admin = (current_tenant_id() = any (auth_details.tenant_administrators)) end
  order by last_name, first_name
$$;
COMMENT ON FUNCTION public.filtered_people IS '@simpleCollections only';
GRANT ALL ON FUNCTION public.filtered_people TO anonymous;

drop function if exists public.event_instance_approx_price;

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
      sum((tenant_trainer.member_price_45min).amount * duration / 45 / num_participants)::numeric(19,4) as amount,
      (tenant_trainer.member_price_45min).currency as currency
    from event_instance_trainer join tenant_trainer on event_instance_trainer.person_id=tenant_trainer.person_id
    where active and event_instance_trainer.instance_id=v_instance.id and tenant_trainer.tenant_id = event_instance_trainer.tenant_id
    group by (tenant_trainer.member_price_45min).currency;
  else
    return query
    select
      sum((tenant_trainer.member_price_45min).amount * duration / 45 / num_participants)::numeric(19,4) as amount,
      (tenant_trainer.member_price_45min).currency as currency
    from event_trainer join tenant_trainer on event_trainer.person_id=tenant_trainer.person_id
    where active and event_trainer.event_id=v_instance.event_id and tenant_trainer.tenant_id = event_trainer.tenant_id
    group by (tenant_trainer.member_price_45min).currency;
  end if;
end;
$$;

select verify_function('event_instance_approx_price');

grant all on function event_instance_approx_price to anonymous;

COMMENT ON FUNCTION event_instance_approx_price IS '@simpleCollections only';


CREATE or replace FUNCTION app_private.tg_event_instance__delete_payment_on_cancellation() RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER AS $$
declare
  payment_id bigint;
begin
  delete from payment where event_instance_id = OLD.id;

  if not new.is_cancelled then
    select (create_event_instance_payment(event_instance)).id into payment_id
    from event_instance join event on event.id=event_id
    where type='lesson'
      and event_instance.id = NEW.id
      and not event_instance.is_cancelled
      and event_instance.since < now()
      and payment_type = 'after_instance'
      and not exists (
        select * from payment where event_instance_id=event_instance.id
      );

    update payment set status ='unpaid' where id = payment_id;
    perform resolve_payment_with_credit(payment.*) from payment where id = payment_id;
  end if;

  return OLD;
end;
$$;

select verify_function('app_private.tg_event_instance__delete_payment_on_cancellation', 'event_instance');

DROP TRIGGER IF EXISTS _500_delete_on_cancellation on public.event_instance;

CREATE TRIGGER _500_delete_on_cancellation
  AFTER UPDATE ON public.event_instance
  FOR EACH ROW
  WHEN (OLD.is_cancelled IS DISTINCT FROM NEW.is_cancelled)
  EXECUTE FUNCTION app_private.tg_event_instance__delete_payment_on_cancellation();
