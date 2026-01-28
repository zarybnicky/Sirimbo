CREATE FUNCTION app_private.tg_payment__fill_accounting_period() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    SET search_path TO 'pg_catalog', 'public', 'pg_temp'
    AS $$
declare
  v_now  timestamptz := CURRENT_TIMESTAMP;
  since  timestamptz;
begin
  if NEW.tenant_id is null then
    raise exception 'payment.tenant_id must be set before tg_payment__fill_accounting_period';
  end if;

  if NEW.accounting_period_id is null then
    select ap.id
    into NEW.accounting_period_id
    from public.accounting_period ap
    where ap.tenant_id = NEW.tenant_id
      and ap.range @> v_now
    order by lower(ap.range) desc
    limit 1;

    if not found then
      since := case
        when extract(month from v_now) >= 9 then date_trunc('year', v_now) + interval '8 months'
        else date_trunc('year', v_now) + interval '8 months' - interval '1 year'
      end;

      begin
        insert into public.accounting_period (tenant_id, name, since, until)
        values (
          NEW.tenant_id,
          'Školní rok ' || extract(year from since),
          since,
          since + interval '12 months' - interval '1 day'
        )
        returning id into NEW.accounting_period_id;
      exception
        when exclusion_violation or unique_violation then
          select ap.id
          into NEW.accounting_period_id
          from public.accounting_period ap
          where ap.tenant_id = NEW.tenant_id
            and ap.range @> v_now
          order by lower(ap.range) desc
          limit 1;

          if not found then
            raise exception 'Failed to create/find accounting_period for tenant % at %', NEW.tenant_id, v_now;
          end if;
      end;
    end if;
  end if;

  return NEW;
end
$$;
