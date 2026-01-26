
--!include functions/event_remaining_x.sql
--!include functions/reset_password.sql
--!include functions/detach_event_instance.sql

ALTER TABLE public.accounting_period
  drop constraint if exists accounting_period__no_overlap__excl,
  ADD CONSTRAINT accounting_period__no_overlap__excl
    EXCLUDE USING gist (tenant_id WITH =, range WITH &&);

ALTER TABLE public.accounting_period
  drop constraint if exists accounting_period__tenant_since__uk,
  ADD CONSTRAINT accounting_period__tenant_since__uk
    UNIQUE (tenant_id, since);


CREATE OR REPLACE FUNCTION app_private.tg_payment__fill_accounting_period()
  RETURNS trigger
  LANGUAGE plpgsql
  SECURITY DEFINER
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

update public.payment p
set accounting_period_id = ap.id
from public.accounting_period ap
where ap.tenant_id = p.tenant_id
  and ap.range @> p.created_at
  and exists (
    select 1
    from public.accounting_period ap_old
    where ap_old.id = p.accounting_period_id
      and ap_old.tenant_id <> p.tenant_id
  );

WITH needed AS (
  select distinct
    p.tenant_id,
    case
      when extract(month from p.created_at) >= 9
      then date_trunc('year', p.created_at) + interval '8 months'
      else date_trunc('year', p.created_at) + interval '8 months' - interval '1 year'
    end as since
  from public.payment p
    union
  select distinct
    p.tenant_id,
    case
      when extract(month from p.created_at) >= 9
      then date_trunc('year', p.created_at) + interval '8 months'
      else date_trunc('year', p.created_at) + interval '8 months' - interval '1 year'
    end as since
  from public.transaction p
),
     ins AS (
       insert into public.accounting_period (tenant_id, name, since, until)
         select
           n.tenant_id,
           'Školní rok ' || extract(year from n.since),
           n.since::timestamp AT TIME ZONE 'UTC',
           (n.since + interval '12 months' - interval '1 day')::timestamp AT TIME ZONE 'UTC'
         from needed n
         on conflict (tenant_id, since) do nothing
         returning 1
     )
select count(*) as inserted_periods from ins;

update public.payment p
set accounting_period_id = ap.id
from public.accounting_period ap
where ap.tenant_id = p.tenant_id
  and ap.range @> p.created_at
  and exists (
    select 1
    from public.accounting_period ap_old
    where ap_old.id = p.accounting_period_id
      and ap_old.tenant_id <> p.tenant_id
  );

update public.transaction p
set accounting_period_id = ap.id
from public.accounting_period ap
where ap.tenant_id = p.tenant_id
  and ap.range @> p.created_at
  and exists (
    select 1
    from public.accounting_period ap_old
    where ap_old.id = p.accounting_period_id
      and ap_old.tenant_id <> p.tenant_id
  );

do
$$
  begin
    if not exists (select 1 from pg_constraint where conname = 'accounting_period_tenant_id_id_key') then
      ALTER TABLE public.accounting_period
        ADD CONSTRAINT accounting_period_tenant_id_id_key UNIQUE (tenant_id, id);
    end if;
    if not exists (select 1 from pg_constraint where conname = 'payment_tenant_id_accounting_period_fkey') then
      ALTER TABLE public.payment
        ADD CONSTRAINT payment_tenant_id_accounting_period_fkey
          FOREIGN KEY (tenant_id, accounting_period_id)
            REFERENCES public.accounting_period (tenant_id, id)
            ON UPDATE CASCADE
            ON DELETE RESTRICT;
    end if;

    if not exists (select 1 from pg_constraint where conname = 'event_tenant_id_id_key') then
      ALTER TABLE public.event        ADD CONSTRAINT event_tenant_id_id_key UNIQUE (tenant_id, id);
    end if;
    if not exists (select 1 from pg_constraint where conname = 'cohort_tenant_id_id_key') then
      ALTER TABLE public.cohort       ADD CONSTRAINT cohort_tenant_id_id_key UNIQUE (tenant_id, id);
    end if;
    if not exists (select 1 from pg_constraint where conname = 'account_tenant_id_id_key') then
      ALTER TABLE public.account      ADD CONSTRAINT account_tenant_id_id_key UNIQUE (tenant_id, id);
    end if;
    if not exists (select 1 from pg_constraint where conname = 'payment_tenant_id_id_key') then
      ALTER TABLE public.payment      ADD CONSTRAINT payment_tenant_id_id_key UNIQUE (tenant_id, id);
    end if;
    if not exists (select 1 from pg_constraint where conname = 'transaction_tenant_id_id_key') then
      ALTER TABLE public.transaction  ADD CONSTRAINT transaction_tenant_id_id_key UNIQUE (tenant_id, id);
    end if;
    if not exists (select 1 from pg_constraint where conname = 'cohort_group_tenant_id_id_key') then
      ALTER TABLE public.cohort_group  ADD CONSTRAINT cohort_group_tenant_id_id_key UNIQUE (tenant_id, id);
    end if;
  end;
$$;

ALTER TABLE public.transaction
  DROP CONSTRAINT IF EXISTS transaction_payment_id_fkey,
  drop constraint if exists transaction_payment_fkey,
  ADD CONSTRAINT transaction_payment_fkey
    FOREIGN KEY (tenant_id, payment_id)
      REFERENCES public.payment (tenant_id, id)
      ON UPDATE CASCADE
      ON DELETE CASCADE;

comment on constraint transaction_payment_fkey
  on public.transaction
  is E'@fieldName payment
@foreignFieldName transactions';

ALTER TABLE public.transaction
  DROP CONSTRAINT IF EXISTS transaction_accounting_period_id_fkey,
  drop constraint if exists transaction_accounting_period_fkey,
  ADD CONSTRAINT transaction_accounting_period_fkey
    FOREIGN KEY (tenant_id, accounting_period_id)
      REFERENCES public.accounting_period (tenant_id, id)
      ON UPDATE CASCADE
      ON DELETE RESTRICT;

comment on constraint transaction_accounting_period_fkey
  on public.transaction
  is E'@fieldName accountingPeriod
@foreignFieldName transactions';

ALTER TABLE public.posting
  DROP CONSTRAINT IF EXISTS posting_transaction_id_fkey,
  drop constraint if exists posting_transaction_fkey,
  ADD CONSTRAINT posting_transaction_fkey
    FOREIGN KEY (tenant_id, transaction_id)
      REFERENCES public.transaction (tenant_id, id)
      ON UPDATE CASCADE
      ON DELETE CASCADE;

comment on constraint posting_transaction_fkey
  on public.posting
  is E'@fieldName transaction
@foreignFieldName postings';

ALTER TABLE public.posting
  DROP CONSTRAINT IF EXISTS posting_account_id_fkey,
  drop constraint if exists posting_account_fkey,
  ADD CONSTRAINT posting_account_fkey
    FOREIGN KEY (tenant_id, account_id)
      REFERENCES public.account (tenant_id, id)
      ON UPDATE CASCADE
      ON DELETE RESTRICT;

comment on constraint posting_account_fkey
  on public.posting
  is E'@fieldName account
@foreignFieldName postings';

ALTER TABLE public.event_instance
  DROP CONSTRAINT IF EXISTS event_instance_event_id_fkey,
  drop constraint if exists event_instance_event_fkey,
  ADD CONSTRAINT event_instance_event_fkey
    FOREIGN KEY (tenant_id, event_id)
      REFERENCES public.event (tenant_id, id)
      ON UPDATE CASCADE
      ON DELETE CASCADE;

comment on constraint event_instance_event_fkey
  on public.event_instance
  is E'@fieldName event
@foreignFieldName eventInstances';

ALTER TABLE public.event_registration
  DROP CONSTRAINT IF EXISTS event_registration_event_id_fkey,
  drop constraint if exists event_registration_event_fkey,
  ADD CONSTRAINT event_registration_event_fkey
    FOREIGN KEY (tenant_id, event_id)
      REFERENCES public.event (tenant_id, id)
      ON UPDATE CASCADE
      ON DELETE CASCADE;

comment on constraint event_registration_event_fkey
  on public.event_registration
  is E'@fieldName event
@foreignFieldName eventRegistrations';

ALTER TABLE public.event_target_cohort
  DROP CONSTRAINT IF EXISTS event_target_cohort_event_id_fkey,
  DROP CONSTRAINT IF EXISTS event_target_cohort_cohort_id_fkey,
  drop constraint if exists event_target_cohort_event_fkey,
  drop constraint if exists event_target_cohort_cohort_fkey,
  ADD CONSTRAINT event_target_cohort_event_fkey
    FOREIGN KEY (tenant_id, event_id)
      REFERENCES public.event (tenant_id, id)
      ON UPDATE CASCADE
      ON DELETE CASCADE,
  ADD CONSTRAINT event_target_cohort_cohort_fkey
    FOREIGN KEY (tenant_id, cohort_id)
      REFERENCES public.cohort (tenant_id, id)
      ON UPDATE CASCADE
      ON DELETE RESTRICT;

comment on constraint event_target_cohort_event_fkey
  on public.event_target_cohort
  is E'@fieldName event
@foreignFieldName eventTargetCohorts';

comment on constraint event_target_cohort_cohort_fkey
  on public.event_target_cohort
  is E'@fieldName cohort
@foreignFieldName eventTargetCohorts';

ALTER TABLE public.cohort
  DROP CONSTRAINT IF EXISTS cohort_cohort_group_id_fkey,
  drop constraint if exists cohort_cohort_group_fkey,
  ADD CONSTRAINT cohort_cohort_group_fkey
    FOREIGN KEY (tenant_id, cohort_group_id)
      REFERENCES public.cohort_group (tenant_id, id)
      ON UPDATE CASCADE
      ON DELETE SET NULL;

comment on constraint cohort_cohort_group_fkey
  on public.cohort
  is E'@fieldName cohortGroup
@foreignFieldName cohorts';

ALTER TABLE public.cohort_membership
  DROP CONSTRAINT IF EXISTS cohort_membership_cohort_id_fkey,
  drop constraint if exists cohort_membership_cohort_fkey,
  ADD CONSTRAINT cohort_membership_cohort_fkey
    FOREIGN KEY (tenant_id, cohort_id)
      REFERENCES public.cohort (tenant_id, id)
      ON UPDATE CASCADE
      ON DELETE CASCADE;
comment on constraint cohort_membership_cohort_fkey
  on public.cohort_membership
  is E'@fieldName cohort
@foreignFieldName cohortMemberships';

ALTER TABLE public.cohort_subscription
  DROP CONSTRAINT IF EXISTS cohort_subscription_cohort_id_fkey,
  DROP CONSTRAINT IF EXISTS cohort_subscription_account_id_fkey,
  drop constraint if exists cohort_subscription_cohort_fkey,
  drop constraint if exists cohort_subscription_account_fkey,
  ADD CONSTRAINT cohort_subscription_cohort_fkey
    FOREIGN KEY (tenant_id, cohort_id)
      REFERENCES public.cohort (tenant_id, id)
      ON UPDATE CASCADE
      ON DELETE CASCADE,
  ADD CONSTRAINT cohort_subscription_account_fkey
    FOREIGN KEY (tenant_id, account_id)
      REFERENCES public.account (tenant_id, id)
      ON UPDATE CASCADE
      ON DELETE CASCADE;
comment on constraint cohort_subscription_account_fkey
  on public.cohort_subscription
  is E'@fieldName account
@foreignFieldName cohortSubscriptions';

comment on constraint cohort_subscription_cohort_fkey
  on public.cohort_subscription
  is E'@fieldName cohort
@foreignFieldName cohortSubscriptions';
