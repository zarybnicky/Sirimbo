--! Previous: sha1:0303c38a0cb29f1f58b44f9a58d138de93e2f993
--! Hash: sha1:d253e25f3d5fd5d9a36a02242b9203dd1ba8d206

--! split: 1-current.sql
CREATE or replace FUNCTION event_trainer_lessons_remaining(e event_trainer) RETURNS integer AS $$
  select e.lessons_offered - (
    select coalesce(sum(lesson_count), 0)
    from event_lesson_demand where trainer_id = e.id
  );
$$ LANGUAGE sql STABLE;

CREATE or replace FUNCTION event_remaining_person_spots(e event) RETURNS integer AS $$
  select e.capacity - (
    select coalesce(sum(case when couple_id is not null then 2 else 1 end), 0)
    from event_registration where event_id = e.id
  ) - (
    select coalesce(count(id), 0)
    from event_external_registration where event_id = e.id
  );
$$ LANGUAGE sql STABLE security definer;

CREATE or replace FUNCTION event_remaining_lessons(e event) RETURNS integer AS $$
  select (
    select coalesce(sum(lessons_offered), 0) from event_trainer et where et.event_id = e.id
  ) - (
    select coalesce(sum(lesson_count), 0) from event_registration er join event_lesson_demand eld on eld.registration_id = er.id where er.event_id = e.id
  );
$$ LANGUAGE sql STABLE security definer;

GRANT ALL ON FUNCTION event_trainer_lessons_remaining(e event_trainer) TO anonymous;
GRANT ALL ON FUNCTION event_remaining_person_spots(e event) TO anonymous;
GRANT ALL ON FUNCTION event_remaining_lessons(e event) TO anonymous;

CREATE or replace FUNCTION public.reset_password(email character varying) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    SET search_path TO pg_catalog, public, pg_temp
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
        select jsonb_agg(person.name)
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

CREATE OR REPLACE FUNCTION public.detach_event_instance(
  p_instance_id     bigint,
  p_tenant_id       bigint DEFAULT public.current_tenant_id(),
  p_new_event_name  text   DEFAULT NULL
)
  RETURNS event
  LANGUAGE plpgsql
AS $$
DECLARE
  v_old_event_id bigint;
  v_new_event_id bigint;
  v_new_event event;
BEGIN
  -- Lock the instance and fetch old event_id
  SELECT ei.event_id
  INTO v_old_event_id
  FROM public.event_instance ei
  WHERE ei.tenant_id = p_tenant_id
    AND ei.id        = p_instance_id
    FOR UPDATE;

  IF v_old_event_id IS NULL THEN
    RAISE EXCEPTION 'detach_event_instance: instance % not found in tenant %', p_instance_id, p_tenant_id;
  END IF;

  -- Also lock the parent event row (prevents concurrent edits during cloning)
  PERFORM 1
  FROM public.event e
  WHERE e.tenant_id = p_tenant_id
    AND e.id        = v_old_event_id
    FOR UPDATE;

  -- Snapshot attendance state for this instance so we can restore status/note later
  CREATE TEMP TABLE _old_att (
     reg_person_id        bigint,
     reg_couple_id        bigint,
     attendee_person_id   bigint NOT NULL,
     status               public.attendance_type NOT NULL,
     note                 text
  ) ON COMMIT DROP;

  PERFORM plpgsql_check_pragma('disable:check');
  INSERT INTO _old_att (reg_person_id, reg_couple_id, attendee_person_id, status, note)
  SELECT er.person_id, er.couple_id, ea.person_id, ea.status, ea.note
  FROM public.event_attendance ea
  JOIN public.event_registration er ON er.tenant_id = ea.tenant_id AND er.id = ea.registration_id AND er.event_id  = ea.event_id
  WHERE ea.tenant_id   = p_tenant_id
    AND ea.instance_id = p_instance_id
    AND ea.event_id    = v_old_event_id;
  PERFORM plpgsql_check_pragma('enable:check');

  -- Critical: delete instance attendance BEFORE changing instance.event_id
  DELETE FROM public.event_attendance ea
  WHERE ea.tenant_id   = p_tenant_id
    AND ea.instance_id = p_instance_id
    AND ea.event_id    = v_old_event_id;

  -- Clone the event (explicit column list, but short and stable in compact.sql)
  INSERT INTO public.event (
    name, location_text, description, capacity, files_legacy,
    updated_at, is_locked, is_visible, summary, is_public, enable_notes, tenant_id,
    type, location_id, created_at
  )
  SELECT
    COALESCE(p_new_event_name, e.name), e.location_text, e.description, e.capacity, e.files_legacy,
    now(), e.is_locked, e.is_visible, e.summary, e.is_public, e.enable_notes, e.tenant_id,
    e.type, e.location_id, now()
  FROM public.event e
  WHERE e.tenant_id = p_tenant_id
    AND e.id        = v_old_event_id
  RETURNING id INTO v_new_event_id;

  -- Copy event_target_cohort (natural key: cohort_id)
  INSERT INTO public.event_target_cohort (tenant_id, event_id, cohort_id, created_at, updated_at)
  SELECT p_tenant_id, v_new_event_id, etc.cohort_id, now(), now()
  FROM public.event_target_cohort etc
  WHERE etc.tenant_id = p_tenant_id
    AND etc.event_id  = v_old_event_id
  ON CONFLICT (event_id, cohort_id) DO NOTHING;

  -- Copy event_trainer (natural key: person_id)
  INSERT INTO public.event_trainer (tenant_id, event_id, person_id, created_at, updated_at, lessons_offered)
  SELECT p_tenant_id, v_new_event_id, et.person_id, now(), now(), et.lessons_offered
  FROM public.event_trainer et
  WHERE et.tenant_id = p_tenant_id
    AND et.event_id  = v_old_event_id
  ON CONFLICT (event_id, person_id) DO NOTHING;

  -- Re-point the instance (event_instance_trainer + any other (tenant_id, instance_id, event_id) dependents follow via FK cascade)
  UPDATE public.event_instance ei
  SET event_id   = v_new_event_id
  WHERE ei.tenant_id = p_tenant_id
    AND ei.id        = p_instance_id
    AND ei.event_id  = v_old_event_id;

  -- Copy registrations: map target_cohort_id by cohort_id (old_tc -> new_tc)
  -- This will auto-generate attendance rows via tg_event_registration__create_attendance()
  INSERT INTO public.event_registration (
    tenant_id, event_id, target_cohort_id, couple_id, person_id, note, created_at
  )
  SELECT p_tenant_id, v_new_event_id, new_tc.id, er.couple_id, er.person_id, er.note, er.created_at
  FROM public.event_registration er
  LEFT JOIN public.event_target_cohort old_tc ON old_tc.tenant_id = er.tenant_id AND old_tc.id = er.target_cohort_id
  LEFT JOIN public.event_target_cohort new_tc ON new_tc.tenant_id = er.tenant_id AND new_tc.event_id  = v_new_event_id AND new_tc.cohort_id = old_tc.cohort_id
  WHERE er.tenant_id = p_tenant_id
    AND er.event_id  = v_old_event_id
  ON CONFLICT (event_id, person_id, couple_id) DO NOTHING;

  -- Copy lesson demand:
  -- - trainer maps by person_id (old event_trainer -> new event_trainer)
  -- - registration maps by (person_id, couple_id) to new event_registration
  INSERT INTO public.event_lesson_demand (
    tenant_id, trainer_id, registration_id, lesson_count, created_at, event_id
  )
  SELECT p_tenant_id, new_tr.id, new_reg.id, d.lesson_count, d.created_at, v_new_event_id
  FROM public.event_lesson_demand d
  JOIN public.event_trainer old_tr ON old_tr.tenant_id = p_tenant_id AND old_tr.id = d.trainer_id AND old_tr.event_id  = v_old_event_id
  JOIN public.event_trainer new_tr ON new_tr.tenant_id = p_tenant_id AND new_tr.event_id  = v_new_event_id AND new_tr.person_id = old_tr.person_id
  JOIN public.event_registration old_reg ON old_reg.tenant_id = p_tenant_id AND old_reg.id = d.registration_id AND old_reg.event_id  = v_old_event_id
  JOIN public.event_registration new_reg ON new_reg.tenant_id = p_tenant_id AND new_reg.event_id = v_new_event_id AND new_reg.person_id IS NOT DISTINCT FROM old_reg.person_id AND new_reg.couple_id IS NOT DISTINCT FROM old_reg.couple_id
  WHERE d.tenant_id = p_tenant_id
    AND d.event_id  = v_old_event_id
  ON CONFLICT (registration_id, trainer_id) DO NOTHING;

  -- Restore attendance status/note on the newly generated attendance rows for the detached instance
  PERFORM plpgsql_check_pragma('disable:check');
  UPDATE public.event_attendance ea
  SET status = oa.status, note = oa.note
  FROM _old_att oa
  JOIN public.event_registration new_reg ON new_reg.tenant_id = p_tenant_id AND new_reg.event_id = v_new_event_id AND new_reg.person_id IS NOT DISTINCT FROM oa.reg_person_id AND new_reg.couple_id IS NOT DISTINCT FROM oa.reg_couple_id
  WHERE ea.tenant_id       = p_tenant_id
    AND ea.instance_id     = p_instance_id
    AND ea.event_id        = v_new_event_id
    AND ea.registration_id = new_reg.id
    AND ea.person_id       = oa.attendee_person_id;
  PERFORM plpgsql_check_pragma('enable:check');

  select * into v_new_event from event where id = v_new_event_id;
  RETURN v_new_event;
END;
$$;

grant all on function detach_event_instance to trainer;


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
