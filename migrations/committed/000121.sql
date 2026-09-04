--! Previous: sha1:b36764f99671aab04a196359a3331b7c7261922d
--! Hash: sha1:756b279bd80b272d38031c8c615dda37621f9d3c

--! split: 1-current.sql
update accounting_period
set until = since + interval '12 months'
where until = since + interval '12 months' - interval '1 day';

create or replace function app_private.tg_payment__fill_accounting_period() returns trigger
  language plpgsql security definer
  set search_path to pg_catalog, public, pg_temp
as $$
declare
  v_now timestamptz := current_timestamp;
  since timestamptz;
begin
  if NEW.tenant_id is null then
    raise exception 'payment.tenant_id must be set before tg_payment__fill_accounting_period';
  end if;

  if NEW.accounting_period_id is null then
    select id into NEW.accounting_period_id
    from accounting_period
    where tenant_id = NEW.tenant_id and range @> v_now
    order by lower(range) desc
    limit 1;

    if not found then
      since := case
        when extract(month from v_now) >= 9 then date_trunc('year', v_now) + interval '8 months'
        else date_trunc('year', v_now) + interval '8 months' - interval '1 year'
      end;

      begin
        insert into accounting_period (tenant_id, name, since, until)
        values (
          NEW.tenant_id,
          'Školní rok ' || extract(year from since),
          since,
          since + interval '12 months'
        )
        returning id into NEW.accounting_period_id;
      exception
        when exclusion_violation or unique_violation then
          select id into NEW.accounting_period_id
          from accounting_period
          where tenant_id = NEW.tenant_id and range @> v_now
          order by lower(range) desc
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
insert into file (
  tenant_id,
  object_key,
  name,
  uploaded_by,
  uploaded_at,
  created_at
)

select
  case split_part(object_name, '/', 1)
    when 'tkolymp' then 1
    when 'kometa' then 2
  end,
  object_name,
  regexp_replace(object_name, '^[^/]+/[0-9]+-', ''),
  uploaded_by,
  uploaded_at,
  uploaded_at
from attachment
on conflict (object_key) do nothing;
