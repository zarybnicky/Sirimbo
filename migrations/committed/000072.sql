--! Previous: sha1:c0184a3febe5cf8de247e05b490a981dc8b2cd1a
--! Hash: sha1:904018b6905db4c244d6f25f48099ee9b6f2354e

--! split: 1-current.sql
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
      coalesce(sum((tenant_trainer.member_price_45min).amount * duration / 45 / nullif(num_participants, 0))::numeric(19,4), 'NaN') as amount,
      coalesce((tenant_trainer.member_price_45min).currency, 'CZK') as currency
    from event_instance_trainer join tenant_trainer on event_instance_trainer.person_id=tenant_trainer.person_id
    where active and event_instance_trainer.instance_id=v_instance.id and tenant_trainer.tenant_id = event_instance_trainer.tenant_id
    group by (tenant_trainer.member_price_45min).currency;
  else
    return query
    select
      coalesce(sum((tenant_trainer.member_price_45min).amount * duration / 45 / nullif(num_participants, 0))::numeric(19,4), 'NaN') as amount,
      coalesce((tenant_trainer.member_price_45min).currency, 'CZK') as currency
    from event_trainer join tenant_trainer on event_trainer.person_id=tenant_trainer.person_id
    where active and event_trainer.event_id=v_instance.event_id and tenant_trainer.tenant_id = event_trainer.tenant_id
    group by (tenant_trainer.member_price_45min).currency;
  end if;
end;
$$;

select verify_function('event_instance_approx_price');

grant all on function event_instance_approx_price to anonymous;

COMMENT ON FUNCTION event_instance_approx_price IS '@simpleCollections only';


CREATE OR REPLACE FUNCTION on_update_author_aktuality() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   NEW.at_kdo = current_user_id();
   RETURN NEW;
END;
$$;

BEGIN;
ALTER TABLE aktuality DISABLE TRIGGER ALL;

UPDATE aktuality SET at_foto = null WHERE at_foto = 0;
UPDATE aktuality SET at_foto = null WHERE at_foto_main = 0 OR at_foto_main IS null;

update aktuality set title_photo_url = '/galerie/' || (select gf_path from galerie_foto where id = at_foto_main)
where title_photo_url is null and at_foto_main is not null;

ALTER TABLE aktuality ENABLE TRIGGER ALL;
COMMIT;
