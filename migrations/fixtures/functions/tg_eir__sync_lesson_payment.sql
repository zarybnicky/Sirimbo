create or replace function app_private.tg_eir__sync_lesson_payment()
  returns trigger language plpgsql security definer
  set search_path = pg_catalog, public, pg_temp
as $$
declare
  event_id bigint;
  instance event_instance;
  payment_id bigint;
begin
  for event_id in select distinct unnest(array[old.event_id, new.event_id]) loop
    select * into instance from event_instance where id = event_id for no key update;
    if not found then continue; end if;

    if (
      not exists (select 1 from payment where event_instance_id = instance.id)
      or array(select d.person_id from payment_debtor d join payment p on p.id = d.payment_id
          where p.event_instance_id = instance.id order by d.person_id)
        = array(select distinct r.person_id from event_registration r
          where r.event_id = instance.id and r.registration_status = 'active'
            and r.person_id is not null and not instance.is_cancelled order by r.person_id)
    ) then
      continue;
    end if;

    delete from payment where event_instance_id = instance.id;
    if not instance.is_cancelled and instance.type = 'lesson' and instance.since < now() then
      payment_id := (create_event_instance_payment(instance)).id;
      update payment set status = 'unpaid' where id = payment_id;
      perform resolve_payment_with_credit(payment.*) from payment where id = payment_id;
    end if;
  end loop;
  return null;
end;
$$;

drop trigger if exists _800_sync_lesson_payment on event_registration;
create constraint trigger _800_sync_lesson_payment
  after insert or delete or update of event_id, person_id, registration_status
  on event_registration
  deferrable initially deferred for each row
  execute function app_private.tg_eir__sync_lesson_payment();
