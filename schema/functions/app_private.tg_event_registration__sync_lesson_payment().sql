CREATE FUNCTION app_private.tg_event_registration__sync_lesson_payment() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  affected_event_id bigint;
  instance event;
  payment_id bigint;
begin
  for affected_event_id in select distinct unnest(array[old.event_id, new.event_id]) loop
    select * into instance from event where id = affected_event_id for no key update;
    if not found then continue; end if;

    if (
      not exists (select 1 from payment where event_id = instance.id)
      or array(select d.person_id from payment_debtor d join payment p on p.id = d.payment_id
          where p.event_id = instance.id order by d.person_id)
        = array(select distinct r.person_id from event_registration r
          where r.event_id = instance.id and r.registration_status = 'active'
            and r.person_id is not null and not instance.is_cancelled order by r.person_id)
    ) then
      continue;
    end if;

    delete from payment where event_id = instance.id;
    if not instance.is_cancelled and instance.type = 'lesson' and instance.since < now() then
      payment_id := (create_event_payment(instance)).id;
      update payment set status = 'unpaid' where id = payment_id;
      perform resolve_payment_with_credit(payment.*) from payment where id = payment_id;
    end if;
  end loop;
  return null;
end;
$$;
